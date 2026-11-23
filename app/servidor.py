"""Quadrante: app local (só neste computador, 127.0.0.1).

Serve a página inicial (inserir planilhas, lista de levantamentos, guia), o painel de cada levantamento
e uma pequena API que roda o R:
  POST /api/enviar?nome=<arquivo.xlsx>     corpo = bytes da planilha -> cria levantamento e roda as análises
  GET  /api/levantamentos                  lista com o resumo de cada um
  GET  /api/tarefa/<id>                    andamento de uma tarefa (log do R)
  POST /api/l/<id>/estilo                  grava estilo.json e refaz só as figuras do R
  POST /api/l/<id>/contexto                grava contexto.json e contexto_campo.txt
  POST /api/l/<id>/pedido                  análise sob pedido (R/pedido.R): confere pressupostos e roda
  GET  /api/pedidos/catalogo               catálogo das análises sob pedido e sugestões por serviço
  POST /api/l/<id>/leitura                 grava leitura_especialista.md
  POST /api/l/<id>/refazer                 roda as análises de novo (mesma planilha)
  POST /api/l/<id>/planilha?nome=          troca a planilha pela versão corrigida (guarda a anterior) e roda de novo
  POST /api/l/<id>/correcoes               grava correcoes.json (correções feitas no painel) e roda de novo
  POST /api/l/<id>/pasta                   abre a pasta do levantamento no Explorer
Só biblioteca padrão do Python. Rodar com:  python -I app/servidor.py
"""
import json, os, re, shutil, subprocess, sys, threading, time, unicodedata, uuid, webbrowser
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
from urllib.parse import urlparse, parse_qs, unquote

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LEV = os.path.join(RAIZ, "levantamentos")
def acha_rscript():
    """Rscript: variável HERP_RSCRIPT, o R mais novo em Arquivos de Programas, ou o que estiver no PATH."""
    import glob
    if os.environ.get("HERP_RSCRIPT") and os.path.exists(os.environ["HERP_RSCRIPT"]):
        return os.environ["HERP_RSCRIPT"]
    cands = []
    for base in [os.environ.get("ProgramFiles", r"C:\Program Files"), os.environ.get("LOCALAPPDATA", "") + r"\Programs"]:
        cands += glob.glob(os.path.join(base, "R", "R-*", "bin", "x64", "Rscript.exe")) + glob.glob(os.path.join(base, "R", "R-*", "bin", "Rscript.exe"))
    def versao(c):
        m = re.search(r"R-(\d+)\.(\d+)\.(\d+)", c)
        return tuple(int(x) for x in m.groups()) if m else (0,)
    if cands:
        return sorted(cands, key=lambda c: (versao(c), "x64" in c))[-1]
    return shutil.which("Rscript") or "Rscript"


RSCRIPT = acha_rscript()
PORTA = int(os.environ.get("HERP_PORTA", "8810"))
MAX_UPLOAD = 60 * 1024 * 1024
os.makedirs(LEV, exist_ok=True)

TAREFAS = {}
PING = {"ultimo": None}   # sinal de vida das páginas abertas (a janela do programa)
TRAVA = threading.Lock()
ACOES_COM_R = ("refazer", "planilha", "correcoes", "estilo", "pedido", "relatorio")   # rodam o R na pasta do levantamento: uma de cada vez
RESERVADOS = set()   # levantamentos com um desses pedidos sendo atendido agora (antes de a tarefa entrar em TAREFAS)


def ocupado(lid):
    return any(t["levantamento"] == lid and t["estado"] == "rodando" for t in TAREFAS.values())


def slug(nome):
    s = unicodedata.normalize("NFKD", nome).encode("ascii", "ignore").decode()
    s = re.sub(r"\.(xlsx|xls)$", "", s, flags=re.I)
    s = re.sub(r"[^A-Za-z0-9]+", "-", s).strip("-").lower()[:50] or "levantamento"
    base, i = s, 2
    while os.path.exists(os.path.join(LEV, s)):
        s = f"{base}-{i}"; i += 1
    return s


def valida_regiao(corpo):
    """Centro e raio dos dados secundários. Devolve o dicionário limpo ou o motivo da recusa (texto)."""
    try:
        j = json.loads(corpo)
    except (ValueError, UnicodeDecodeError):
        return "A região não veio em formato válido."
    if not isinstance(j, dict):
        return "A região não veio em formato válido."
    try:
        lat, lon, raio = float(j.get("lat")), float(j.get("lon")), float(j.get("raio_km"))
    except (TypeError, ValueError):
        return "Informe latitude, longitude e raio em números."
    if not (-35 <= lat <= 7 and -75 <= lon <= -28):
        return "O centro precisa ficar no Brasil (latitude entre -35 e 7, longitude entre -75 e -28)."
    if not 1 <= raio <= 500:
        return "O raio precisa ficar entre 1 e 500 km."
    origem = j.get("origem") if j.get("origem") in ("pontos", "municipio", "digitado", "kml") else "digitado"
    txt = lambda k, n: str(j.get(k) or "")[:n]
    return {"lat": round(lat, 6), "lon": round(lon, 6), "raio_km": raio, "origem": origem, "municipio": txt("municipio", 120),
            "uf": txt("uf", 2).upper(), "datum": txt("datum", 40) or "SIRGAS 2000", "salvo_em": time.strftime("%Y-%m-%d %H:%M:%S")}


def pasta_ok(lid):
    if not re.fullmatch(r"[a-z0-9-]{1,60}", lid or ""):
        return None
    p = os.path.join(LEV, lid)
    return p if os.path.isdir(p) else None



def xlsx_simples(caminho, abas):
    """Grava um .xlsx só com a biblioteca padrão. abas = [(nome, [linha0 (cabeçalho), linha1, ...]), ...].
    Números escritos como "12", "0,53" ou "42%" viram números de verdade (a % com formato de porcentagem)."""
    import zipfile
    from xml.sax.saxutils import escape, quoteattr
    import math

    def col(i):
        n = ""
        i += 1
        while i:
            i, r = divmod(i - 1, 26); n = chr(65 + r) + n
        return n

    def celula(ref, v, cab):
        t = "" if v is None else str(v).strip()
        if cab:
            return f'<c r="{ref}" t="inlineStr" s="2"><is><t xml:space="preserve">{escape(t)}</t></is></c>'
        if isinstance(v, (int, float)) and not isinstance(v, bool) and math.isfinite(v):
            return f'<c r="{ref}"><v>{v}</v></c>'
        # Seleções antigas contêm tanto números JS quanto texto formatado em português.
        # Preserve como texto uma cadeia ambígua, como "1.234", em vez de mudar sua escala.
        if re.fullmatch(r"-?\d+\.\d+", t):
            if re.fullmatch(r"-?[1-9]\d{0,2}\.\d{3}", t):
                return f'<c r="{ref}" t="inlineStr"><is><t>{t}</t></is></c>'
            return f'<c r="{ref}"><v>{float(t)}</v></c>'
        m = re.fullmatch(r"(-?\d{1,3}(?:\.\d{3})+|-?\d+)(,\d+)?(%)?", t)
        if m and t not in ("-", ""):
            num = float(m.group(1).replace(".", "") + (m.group(2) or "").replace(",", "."))
            if m.group(3):
                return f'<c r="{ref}" s="{3 if m.group(2) else 1}"><v>{num / 100}</v></c>'
            return f'<c r="{ref}"><v>{num}</v></c>'
        return f'<c r="{ref}" t="inlineStr"><is><t xml:space="preserve">{escape(t)}</t></is></c>'

    usados, nomes = set(), []
    for nome, _ in abas:
        n = re.sub(r"[\[\]\*\?/\\:]", " ", nome)[:31].strip() or "Tabela"
        base, k = n, 2
        while n.lower() in usados:
            n = f"{base[:28]} {k}"; k += 1
        usados.add(n.lower()); nomes.append(n)
    with zipfile.ZipFile(caminho, "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("[Content_Types].xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
                   '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>'
                   '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
                   '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
                   + "".join(f'<Override PartName="/xl/worksheets/sheet{i + 1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>' for i in range(len(abas)))
                   + "</Types>")
        z.writestr("_rels/.rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                   '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>')
        z.writestr("xl/workbook.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
                   'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>'
                   + "".join(f'<sheet name={quoteattr(n)} sheetId="{i + 1}" r:id="rId{i + 1}"/>' for i, n in enumerate(nomes)) + "</sheets></workbook>")
        z.writestr("xl/_rels/workbook.xml.rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                   + "".join(f'<Relationship Id="rId{i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet{i + 1}.xml"/>' for i in range(len(abas)))
                   + f'<Relationship Id="rId{len(abas) + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/></Relationships>')
        z.writestr("xl/styles.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
                   '<fonts count="2"><font><sz val="11"/><name val="Calibri"/></font><font><b/><sz val="11"/><name val="Calibri"/></font></fonts>'
                   '<fills count="3"><fill><patternFill patternType="none"/></fill><fill><patternFill patternType="gray125"/></fill><fill><patternFill patternType="solid"><fgColor rgb="FFE9E4D6"/></patternFill></fill></fills>'
                   '<borders count="1"><border/></borders><cellStyleXfs count="1"><xf/></cellStyleXfs>'
                   '<cellXfs count="4"><xf/><xf numFmtId="9" applyNumberFormat="1"/><xf fontId="1" fillId="2" applyFont="1" applyFill="1"/><xf numFmtId="10" applyNumberFormat="1"/></cellXfs></styleSheet>')
        for i, (_, linhas) in enumerate(abas):
            larg = {}
            for ln in linhas:
                for j, v in enumerate(ln):
                    larg[j] = min(60, max(larg.get(j, 6), len(str(v if v is not None else "")) + 2))
            cols = "<cols>" + "".join(f'<col min="{j + 1}" max="{j + 1}" width="{w}" customWidth="1"/>' for j, w in sorted(larg.items())) + "</cols>" if larg else ""
            rows = "".join(f'<row r="{r + 1}">' + "".join(celula(f"{col(j)}{r + 1}", v, r == 0) for j, v in enumerate(ln)) + "</row>" for r, ln in enumerate(linhas))
            z.writestr(f"xl/worksheets/sheet{i + 1}.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
                       '<sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>'
                       f'{cols}<sheetData>{rows}</sheetData></worksheet>')
    return nomes

def rodar(cmd, lid, rotulo, env_extra=None):
    tid = uuid.uuid4().hex[:10]
    TAREFAS[tid] = {"id": tid, "levantamento": lid, "rotulo": rotulo, "estado": "rodando", "log": [], "inicio": time.time(), "pct": 0, "etapa": "Começando"}

    def trabalho():
        t = TAREFAS[tid]
        env = dict(os.environ, **(env_extra or {}))
        try:
            p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, cwd=RAIZ, env=env,
                                 creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
            for linha in iter(p.stdout.readline, b""):
                txt = linha.decode("utf-8", "replace").rstrip()
                m = re.match(r"\[\s*(\d+)%\] (.*)", txt) or re.match(r"@@ (\d+) (.*)", txt)
                if m:  # linha de andamento enviada pelo R: "[ 45%] <etapa>"
                    t["pct"] = min(100, int(m.group(1))); t["etapa"] = m.group(2)
                elif txt and "permutations" not in txt and "minperm" not in txt:
                    t["log"].append(txt)
            p.wait()
            t["estado"] = "pronto" if p.returncode == 0 else "erro"
            if p.returncode == 0: t["pct"] = 100; t["etapa"] = "Pronto"
        except Exception as e:  # noqa: BLE001
            t["log"].append(f"Falha ao rodar o R: {e}"); t["estado"] = "erro"
        with open(os.path.join(LEV, lid, "log.txt"), "a", encoding="utf-8") as f:
            f.write(f"\n== {time.strftime('%d/%m/%Y %H:%M')} {rotulo} ({t['estado']})\n" + "\n".join(t["log"]) + "\n")
    threading.Thread(target=trabalho, daemon=True).start()
    return tid


def analisar(lid):
    p = os.path.join(LEV, lid)
    meta = json.load(open(os.path.join(p, "origem.json"), encoding="utf-8"))
    return rodar([RSCRIPT, os.path.join(RAIZ, "R", "quadrante.R"), os.path.join(p, "planilha.xlsx"), p], lid,
                 "Análises", {"HERP_NOME": meta.get("nome", "planilha.xlsx")})


def listar():
    out = []
    for lid in sorted(os.listdir(LEV)):
        p = os.path.join(LEV, lid)
        if not os.path.isdir(p):
            continue
        item = {"id": lid}
        for arq, chave in (("origem.json", "origem"), ("resumo.json", "resumo")):
            f = os.path.join(p, arq)
            if os.path.exists(f):
                try:
                    item[chave] = json.load(open(f, encoding="utf-8"))
                except Exception:  # noqa: BLE001
                    pass
        item["tem_painel"] = os.path.exists(os.path.join(p, "dados.js"))
        item["rodando"] = any(t["levantamento"] == lid and t["estado"] == "rodando" for t in TAREFAS.values())
        out.append(item)
    out.sort(key=lambda x: (x.get("origem") or {}).get("enviado_em", ""), reverse=True)
    return out


class Manipulador(SimpleHTTPRequestHandler):
    def __init__(self, *a, **k):
        super().__init__(*a, directory=RAIZ, **k)

    def log_message(self, *a):
        pass

    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()

    def json(self, obj, cod=200):
        b = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self.send_response(cod); self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(b))); self.end_headers(); self.wfile.write(b)

    def corpo(self):
        n = int(self.headers.get("Content-Length") or 0)
        if n > MAX_UPLOAD:
            raise ValueError("arquivo grande demais")
        return self.rfile.read(n)

    def descarta_corpo(self):
        """Lê e joga fora o corpo (até o limite de envio), para a resposta chegar ao navegador."""
        n = min(int(self.headers.get("Content-Length") or 0), MAX_UPLOAD)
        while n > 0:
            b = self.rfile.read(min(n, 1 << 20))
            if not b:
                break
            n -= len(b)

    def do_GET(self):
        u = urlparse(self.path); caminho = unquote(u.path)
        if caminho in ("/", "/index.html"):
            self.path = "/app/inicio.html"; return super().do_GET()
        if caminho == "/api/pedidos/catalogo":   # análises que o R roda sob pedido (R/pedido.R)
            return self.json(json.load(open(os.path.join(RAIZ, "R", "pedidos_catalogo.json"), encoding="utf-8")))
        if caminho == "/api/versao":   # data do arquivo .R mais novo (o painel compara com figuras/_versao.json)
            rs = [os.path.join(RAIZ, "R", f) for f in os.listdir(os.path.join(RAIZ, "R"))
                  if re.fullmatch(r"(saida.*|fitofisionomias|campanhas|figuras|ajustes_fig)\.R", f)]   # só os arquivos que desenham figuras
            return self.json({"codigo": max((os.path.getmtime(f) for f in rs), default=0)})
        if caminho == "/api/ping":
            PING["ultimo"] = time.time()
            return self.json({"ok": True})
        if caminho == "/api/levantamentos":
            return self.json(listar())
        m = re.fullmatch(r"/api/l/([a-z0-9-]+)/regiao", caminho)
        if m:   # dados secundários: centro e raio salvos (ou {} se ainda não salvos)
            p = pasta_ok(m.group(1))
            if not p:
                return self.send_error(404)
            arq = os.path.join(p, "secundarios", "regiao.json")
            return self.json(json.load(open(arq, encoding="utf-8")) if os.path.isfile(arq) else {})
        m = re.fullmatch(r"/api/tarefa/([a-f0-9]{10})", caminho)
        if m:
            t = TAREFAS.get(m.group(1))
            return self.json(t if t else {"estado": "desconhecida"}, 200 if t else 404)
        m = re.fullmatch(r"/l/([a-z0-9-]+)/?(painel\.html)?", caminho)
        if m:  # o painel é sempre o modelo atual, lendo o dados.js da pasta do levantamento
            if not pasta_ok(m.group(1)):
                return self.send_error(404)
            self.path = "/painel/painel.html"; return super().do_GET()
        m = re.fullmatch(r"/l/([a-z0-9-]+)/(.+)", caminho)
        if m:
            if not pasta_ok(m.group(1)) or ".." in m.group(2):
                return self.send_error(404)
            self.path = "/levantamentos/" + m.group(1) + "/" + m.group(2); return super().do_GET()
        if caminho.startswith(("/app/", "/painel/", "/identidade/")):
            return super().do_GET()
        return self.send_error(404)

    def do_POST(self):
        u = urlparse(self.path); caminho = unquote(u.path)
        if self.headers.get("Origin") not in (None, f"http://127.0.0.1:{PORTA}", f"http://localhost:{PORTA}"):
            return self.send_error(403)
        reservou = None
        rel_temporario = None
        try:
            if caminho == "/api/enviar":
                nome = os.path.basename(parse_qs(u.query).get("nome", ["planilha.xlsx"])[0])
                if not re.search(r"\.(xlsx|xls)$", nome, re.I):
                    return self.json({"erro": "Envie uma planilha .xlsx ou .xls."}, 400)
                dados = self.corpo()
                if dados[:2] != b"PK" and dados[:4] != b"\xd0\xcf\x11\xe0":
                    return self.json({"erro": "O arquivo não parece ser uma planilha do Excel."}, 400)
                with TRAVA:
                    lid = slug(nome); p = os.path.join(LEV, lid); os.makedirs(p)
                with open(os.path.join(p, "planilha.xlsx"), "wb") as f:
                    f.write(dados)
                json.dump({"nome": nome, "enviado_em": time.strftime("%Y-%m-%d %H:%M:%S"), "titulo": re.sub(r"\.(xlsx|xls)$", "", nome, flags=re.I)},
                          open(os.path.join(p, "origem.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"id": lid, "tarefa": analisar(lid)})
            m = re.fullmatch(r"/api/l/([a-z0-9-]+)/(estilo|contexto|leitura|refazer|pasta|titulo|selecao|relatorio|planilha|correcoes|pedido|regiao|area)", caminho)
            if not m:
                return self.send_error(404)
            lid, acao = m.groups(); p = pasta_ok(lid)
            if not p:
                return self.send_error(404)
            if acao in ACOES_COM_R:
                # 19/11/2026: dois pedidos seguidos (clique duplo em "Rodar de novo") punham dois R gravando na mesma pasta
                with TRAVA:
                    livre = lid not in RESERVADOS and not ocupado(lid)
                    if livre:
                        RESERVADOS.add(lid)
                if not livre:
                    self.descarta_corpo()   # sem ler o corpo, o navegador vê a conexão cortada em vez da resposta
                    return self.json({"erro": "As análises deste levantamento ainda estão rodando no R. Espere terminar e tente de novo."}, 409)
                reservou = lid   # liberado no finally, depois que a tarefa já está em TAREFAS
            if acao == "pasta":
                sub = parse_qs(u.query).get("sub", [""])[0]
                alvo = os.path.join(p, sub) if sub in ("relatorio", "figuras", "tabelas") and os.path.isdir(os.path.join(p, sub)) else p
                os.startfile(alvo); return self.json({"ok": True})
            if acao == "selecao":
                sel = json.loads(self.corpo().decode("utf-8"))
                json.dump(sel, open(os.path.join(p, "selecao.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"ok": True})
            if acao == "relatorio":
                sel = json.loads(self.corpo().decode("utf-8"))
                if not isinstance(sel, list) or not sel or not all(isinstance(it, dict) and it.get("tipo") in ("fig", "tab") for it in sel):
                    return self.json({"erro": "Selecione ao menos uma figura ou tabela válida."}, 400)
                import tempfile
                rel = rel_temporario = tempfile.mkdtemp(prefix=".relatorio_", dir=p)
                nfig = ntab = 0; leia = ["Itens do relatório (na ordem escolhida no painel)", ""]
                abas = []; indice = [["Aba", "Tabela"]]
                limpa = lambda t: re.sub(r"[^A-Za-z0-9]+", "_", unicodedata.normalize("NFKD", t).encode("ascii", "ignore").decode()).strip("_")[:60]
                for it in sel:
                    if it.get("tipo") == "fig":
                        orig = os.path.realpath(os.path.join(p, it.get("arquivo", "")))
                        if os.path.commonpath([os.path.realpath(p), orig]) != os.path.realpath(p) or not orig.lower().endswith(".png") or not os.path.isfile(orig):
                            return self.json({"erro": "Uma figura selecionada está indisponível. Confira a seleção antes de exportar."}, 400)
                        nfig += 1; nome = f"Fig{nfig:02d}_{limpa(it.get('titulo', 'figura'))}.png"
                        shutil.copyfile(orig, os.path.join(rel, nome)); leia.append(f"Figura {nfig}. {it.get('titulo', '')}  ->  {nome}")
                        jpg = orig[:-4] + ".jpg"   # JPEG de alta resolução (600 dpi), quando o R gravou
                        if orig.lower().endswith(".png") and os.path.exists(jpg):
                            shutil.copyfile(jpg, os.path.join(rel, nome[:-4] + ".jpg"))
                    elif it.get("tipo") == "tab":
                        ntab += 1
                        abas.append((f"Tab{ntab:02d} {it.get('titulo', 'tabela')}", [it.get("cols", [])] + it.get("linhas", [])))
                        indice.append([f"Tab{ntab:02d}", it.get("titulo", "")])
                        leia.append(f"Tabela {ntab}. {it.get('titulo', '')}  ->  aba Tab{ntab:02d} de Tabelas_do_relatorio.xlsx")
                if abas:
                    xlsx_simples(os.path.join(rel, "Tabelas_do_relatorio.xlsx"), [("Índice", indice)] + abas)
                open(os.path.join(rel, "LEIA-ME.txt"), "w", encoding="utf-8").write("\n".join(leia) + "\n\nTodas as tabelas estão numa planilha só (Tabelas_do_relatorio.xlsx), uma aba por tabela; a aba Índice lista todas.\n")
                destino = os.path.join(p, "relatorio")
                anterior = None
                if os.path.isdir(destino):
                    anterior = os.path.join(p, "relatorio_anterior_" + time.strftime("%Y%m%d_%H%M%S") + "_" + uuid.uuid4().hex[:8])
                    os.rename(destino, anterior)
                try:
                    os.rename(rel, destino)
                except OSError:
                    if anterior is not None:
                        os.rename(anterior, destino)
                    raise
                return self.json({"figuras": nfig, "tabelas": ntab, "pasta": destino})
            if acao == "correcoes":
                try:
                    lista = json.loads(self.corpo().decode("utf-8"))
                except ValueError:
                    return self.json({"erro": "Correções inválidas."}, 400)
                if not isinstance(lista, list) or not all(isinstance(c, dict) and c.get("tipo") for c in lista):
                    return self.json({"erro": "Correções inválidas."}, 400)
                json.dump(lista, open(os.path.join(p, "correcoes.json"), "w", encoding="utf-8"), ensure_ascii=False, indent=1)
                return self.json({"tarefa": analisar(lid)})
            if acao == "planilha":
                nome = os.path.basename(parse_qs(u.query).get("nome", ["planilha.xlsx"])[0])
                dados = self.corpo()
                if not re.search(r"\.(xlsx|xls)$", nome, re.I) or (dados[:2] != b"PK" and dados[:4] != b"\xd0\xcf\x11\xe0"):
                    return self.json({"erro": "Envie uma planilha do Excel (.xlsx)."}, 400)
                atual = os.path.join(p, "planilha.xlsx")
                if os.path.exists(atual):
                    shutil.copyfile(atual, os.path.join(p, time.strftime("planilha_anterior_%Y%m%d_%H%M%S.xlsx")))
                with open(atual, "wb") as f:
                    f.write(dados)
                try:
                    o = json.load(open(os.path.join(p, "origem.json"), encoding="utf-8"))
                except Exception:  # noqa: BLE001
                    o = {}
                o.update({"nome": nome, "atualizado_em": time.strftime("%Y-%m-%d %H:%M:%S")})
                json.dump(o, open(os.path.join(p, "origem.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"tarefa": analisar(lid)})
            if acao == "refazer":
                return self.json({"tarefa": analisar(lid)})
            if acao == "area":   # KML/KMZ da área: o centro vira o centro da região dos dados secundários
                nome = os.path.basename(parse_qs(u.query).get("nome", ["area.kml"])[0])
                if not re.search(r"\.(kml|kmz)$", nome, re.I):
                    return self.json({"erro": "Envie o arquivo da área em KML ou KMZ (Google Earth, QGIS)."}, 400)
                dados = self.corpo()
                try:
                    sys.path.insert(0, os.path.join(RAIZ, "app")) if os.path.join(RAIZ, "app") not in sys.path else None
                    import area_kml   # python -I não põe a pasta do script no caminho
                    a = area_kml.ler_area(dados, nome)
                except ValueError as e:
                    return self.json({"erro": str(e)}, 400)
                d = os.path.join(p, "secundarios"); os.makedirs(d, exist_ok=True)
                ext = ".kmz" if nome.lower().endswith(".kmz") else ".kml"
                for velho in ("area.kml", "area.kmz"):
                    if os.path.exists(os.path.join(d, velho)):
                        os.remove(os.path.join(d, velho))
                with open(os.path.join(d, "area" + ext), "wb") as f:
                    f.write(dados)
                a["arquivo"] = nome
                json.dump(a, open(os.path.join(d, "area.json"), "w", encoding="utf-8"), ensure_ascii=False, indent=1)
                arq_reg = os.path.join(d, "regiao.json")
                try:
                    reg = json.load(open(arq_reg, encoding="utf-8"))
                except Exception:  # noqa: BLE001
                    reg = {"raio_km": 50, "uf": "", "municipio": "", "datum": "SIRGAS 2000"}
                reg.update({"lat": a["lat"], "lon": a["lon"], "origem": "kml", "area": a, "salvo_em": time.strftime("%Y-%m-%d %H:%M:%S")})
                json.dump(reg, open(arq_reg, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
                return self.json(a)
            corpo = self.corpo().decode("utf-8")
            if acao == "estilo":
                json.loads(corpo)
                open(os.path.join(p, "estilo.json"), "w", encoding="utf-8").write(corpo)
                if not os.path.exists(os.path.join(p, "resultados.rds")):
                    return self.json({"tarefa": analisar(lid)})
                so = parse_qs(u.query).get("so", [""])[0]
                so = so if re.fullmatch(r"[a-z_]{1,40}", so) else ""
                return self.json({"tarefa": rodar([RSCRIPT, os.path.join(RAIZ, "R", "figuras.R"), p] + ([so] if so else []), lid,
                                                  "Figura " + so if so else "Figuras")})
            if acao == "pedido":   # análise sob pedido: o R confere os pressupostos e só roda se passarem
                j = json.loads(corpo)
                ids = {a["id"] for a in json.load(open(os.path.join(RAIZ, "R", "pedidos_catalogo.json"), encoding="utf-8"))["analises"]}
                an, gr = j.get("analise", ""), str(j.get("grupo", ""))[:60]
                fa = j.get("fator") or "-"; va = j.get("variavel") or "riqueza"
                if an == "regressao_gradiente":
                    fator_valido = isinstance(fa, str) and re.fullmatch(r"amb_[0-9a-f]{32}", fa)
                else:
                    fator_valido = fa in ("-", "fitofisionomia", "grupo_amostral", "campanha", "estacao")
                if an not in ids or not gr or not fator_valido or va not in ("riqueza", "abundancia"):
                    return self.json({"erro": "Pedido de análise inválido."}, 400)
                if not os.path.exists(os.path.join(p, "resultados.rds")):
                    return self.json({"erro": "Rode as análises deste levantamento primeiro."}, 400)
                return self.json({"tarefa": rodar([RSCRIPT, os.path.join(RAIZ, "R", "pedido.R"), p, an, gr, fa, va], lid, "Pedido: " + an)})
            if acao == "regiao":
                j = valida_regiao(corpo)
                if isinstance(j, str):
                    return self.json({"erro": j}, 400)
                os.makedirs(os.path.join(p, "secundarios"), exist_ok=True)
                json.dump(j, open(os.path.join(p, "secundarios", "regiao.json"), "w", encoding="utf-8"), ensure_ascii=False, indent=1)
                return self.json({"ok": True})
            if acao == "contexto":
                j = json.loads(corpo)
                if not isinstance(j, dict):
                    return self.json({"erro": "Contexto inválido."}, 400)
                linhas = ["SERVIÇO: " + str(j.get("servico", "")), "", "FINALIDADE", str(j.get("finalidade", "")), "", "COMO FOI O CAMPO", str(j.get("campo", ""))]
                for c in j.get("campanhas") or []:
                    linhas += ["", "CAMPANHA " + str(c.get("nome", "")), "  " + "; ".join(f"{k}: {v}" for k, v in c.items() if k != "nome" and v not in ("", None, []))]
                open(os.path.join(p, "contexto_campo.txt"), "w", encoding="utf-8").write("\n".join(linhas) + "\n")
                json.dump(j, open(os.path.join(p, "contexto.json"), "w", encoding="utf-8"), ensure_ascii=False)
                return self.json({"ok": True})
            if acao == "leitura":
                open(os.path.join(p, "leitura_especialista.md"), "w", encoding="utf-8").write(corpo)
                return self.json({"ok": True})
            if acao == "titulo":
                j = json.loads(corpo); f = os.path.join(p, "origem.json"); o = json.load(open(f, encoding="utf-8"))
                o["titulo"] = j.get("titulo", o.get("titulo", ""))[:120]
                json.dump(o, open(f, "w", encoding="utf-8"), ensure_ascii=False); return self.json({"ok": True})
        except (ValueError, json.JSONDecodeError) as e:
            return self.json({"erro": str(e)}, 400)
        finally:
            if rel_temporario and os.path.isdir(rel_temporario):
                shutil.rmtree(rel_temporario, ignore_errors=True)
            if reservou:
                with TRAVA:
                    RESERVADOS.discard(reservou)


def ja_aberto(url):
    """True se o Quadrante já está rodando nesta porta (segundo clique no atalho)."""
    import urllib.request
    try:
        with urllib.request.urlopen(url + "api/levantamentos", timeout=2) as r:
            return r.status == 200
    except Exception:  # noqa: BLE001
        return False


def acha_navegador_app():
    """Edge (vem no Windows) ou Chrome, para abrir em janela de programa (--app), sem abas nem barra de endereço."""
    import glob
    cands = []
    for base in [os.environ.get("ProgramFiles(x86)", r"C:\Program Files (x86)"), os.environ.get("ProgramFiles", r"C:\Program Files"), os.environ.get("LOCALAPPDATA", "")]:
        cands += [os.path.join(base, "Microsoft", "Edge", "Application", "msedge.exe"), os.path.join(base, "Google", "Chrome", "Application", "chrome.exe")]
    for c in cands:
        if c and os.path.exists(c):
            return c
    return shutil.which("msedge") or shutil.which("chrome")


def abre_janela(url):
    """Abre o Quadrante numa janela própria. Devolve o processo (quando dá para saber quando ela fecha) ou None."""
    exe = acha_navegador_app()
    if not exe:
        webbrowser.open(url)
        return None
    perfil = os.path.join(os.environ.get("LOCALAPPDATA", RAIZ), "Quadrante", "janela")   # perfil só do Quadrante
    os.makedirs(perfil, exist_ok=True)
    try:
        return subprocess.Popen([exe, f"--app={url}", f"--user-data-dir={perfil}", "--window-size=1440,920",
                                 "--no-first-run", "--no-default-browser-check", "--disable-features=Translate"],
                                creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    except OSError:
        webbrowser.open(url)
        return None


def janela_aberta():
    """True se a janela do programa (Edge/Chrome com o perfil próprio do Quadrante) ainda existe.
    O Edge troca de processo ao abrir, então o processo lançado não serve para saber disso."""
    perfil = os.path.join(os.environ.get("LOCALAPPDATA", RAIZ), "Quadrante", "janela")
    cmd = ("Get-CimInstance Win32_Process -Filter \"Name='msedge.exe' or Name='chrome.exe'\" | "
           "Where-Object { $_.CommandLine -like '*" + perfil.replace("'", "''") + "*' -and $_.CommandLine -notlike '*--type=*' } | "
           "Measure-Object | Select-Object -ExpandProperty Count")
    try:
        out = subprocess.run(["powershell", "-NoProfile", "-Command", cmd], capture_output=True, text=True, timeout=20,
                             creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0)).stdout.strip()
        return int(out or 0) > 0
    except Exception:  # noqa: BLE001
        return None   # não deu para saber


def main():
    url = f"http://127.0.0.1:{PORTA}/"
    if ja_aberto(url):
        print("O Quadrante já está aberto; abrindo a janela dele.")
        if "--sem-navegador" not in sys.argv:
            abre_janela(url)
        time.sleep(1)
        sys.exit(3)

    class Servidor(ThreadingHTTPServer):
        allow_reuse_address = False   # no Windows, True deixaria dois programas na mesma porta
    srv = Servidor(("127.0.0.1", PORTA), Manipulador)
    print(f"Quadrante aberto em {url}  (fechar a janela do programa encerra tudo)")
    if "--sem-navegador" not in sys.argv:
        JANELA = {"proc": None}
        threading.Timer(0.6, lambda: JANELA.update(proc=abre_janela(url))).start()

        def vigia():
            # 18/11/2026: o navegador espaça os sinais (1 por minuto ou menos) quando a janela fica minimizada ou atrás de
            # outra, e o programa se fechava com o painel ainda aberto ("O R está redesenhando..." travado).
            # Agora: com a janela do programa aberta (processo vivo), nunca encerra; fechada, encerra 45 s depois do
            # último sinal; sem como saber da janela (navegador comum), só depois de 15 min sem sinal.
            while True:
                time.sleep(5)
                u = PING["ultimo"]
                proc = JANELA["proc"]
                if proc is not None and proc.poll() is None:
                    continue                      # janela do programa aberta
                limite = 45 if proc is not None else 900
                if u is None or time.time() - u < limite:
                    continue
                if proc is not None and janela_aberta() is not False:
                    continue                      # janela minimizada ou atrás de outra: o navegador espaça os sinais
                if any(t.get("estado") == "rodando" for t in TAREFAS.values()):
                    continue
                print("Janela fechada; encerrando o Quadrante.")
                srv.shutdown()
                return
        threading.Thread(target=vigia, daemon=True).start()
    srv.serve_forever()


if __name__ == "__main__":
    main()
