"""Biblioteca de artigos do Quadrante (dados secundários): uma pasta para todos os levantamentos.

biblioteca/pdfs/              artigos já obtidos (o usuário também pode soltar PDFs aqui)
biblioteca/obras.csv          índice (UTF-8, separado por ;)
biblioteca/especies/<id>.csv  lista de espécies já extraída e revisada de cada obra

Antes de buscar um artigo fora, o agente pergunta aqui:
  python -I app/biblioteca.py procurar --doi 10.xxxx/yyy
  python -I app/biblioteca.py procurar --titulo "Anfíbios da Serra ..."
  python -I app/biblioteca.py catalogar               (registra PDFs soltos em pdfs/)
  python -I app/biblioteca.py registrar --json obra.json
Também procura no acervo da Universidade de Anuros (Anuros do Cerrado/02_acervo/obras.csv), quando existe.
Pastas trocáveis por variáveis de ambiente: QUAD_BIBLIOTECA e QUAD_ACERVO (usadas nos testes).
"""
import argparse, csv, json, os, re, sys, time, unicodedata

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COLUNAS = ["id_obra", "doi", "titulo", "referencia", "ano", "tipo", "municipios", "uf", "lat", "lon", "pdf", "lida", "lida_em"]


def pasta():
    return os.environ.get("QUAD_BIBLIOTECA") or os.path.join(RAIZ, "biblioteca")


def acervo():
    return os.environ.get("QUAD_ACERVO") or os.path.join(RAIZ, "..", "..", "..", "Anuros do Cerrado", "02_acervo", "obras.csv")


def norm_doi(d):
    d = (d or "").strip().lower()
    return re.sub(r"^(https?://(dx\.)?doi\.org/|doi:\s*)", "", d)


def norm_tit(t):
    t = unicodedata.normalize("NFKD", t or "").encode("ascii", "ignore").decode().lower()
    return " ".join(re.sub(r"[^a-z0-9]+", " ", t).split())


def carregar():
    arq = os.path.join(pasta(), "obras.csv")
    if not os.path.isfile(arq):
        return []
    with open(arq, encoding="utf-8-sig", newline="") as f:
        return [{c: (r.get(c) or "") for c in COLUNAS} for r in csv.DictReader(f, delimiter=";")]


def gravar(obras):
    os.makedirs(pasta(), exist_ok=True)
    arq = os.path.join(pasta(), "obras.csv")
    with open(arq + ".tmp", "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, COLUNAS, delimiter=";")
        w.writeheader(); w.writerows(obras)
    os.replace(arq + ".tmp", arq)


def _casa(o, doi, titulo):
    if doi and norm_doi(o.get("doi")) == norm_doi(doi):
        return True
    return bool(titulo) and norm_tit(o.get("titulo")) == norm_tit(titulo) != ""


def procurar(doi=None, titulo=None):
    for o in carregar():
        if _casa(o, doi, titulo):
            esp = os.path.join(pasta(), "especies", o["id_obra"] + ".csv")
            return {"achou": True, "origem": "biblioteca", "id_obra": o["id_obra"], "pdf": o["pdf"], "lida": o["lida"],
                    "especies_csv": esp if os.path.isfile(esp) else "", "referencia": o["referencia"] or o["titulo"]}
    arq = acervo()
    if os.path.isfile(arq):
        nt = norm_tit(titulo) if titulo else ""
        with open(arq, encoding="utf-8-sig", newline="") as f:
            for r in csv.DictReader(f, delimiter=";"):
                cit = norm_tit(r.get("citacao"))
                if (doi and norm_doi(r.get("doi")) == norm_doi(doi)) or (len(nt) >= 20 and nt in cit):
                    return {"achou": True, "origem": "acervo", "id_obra": r.get("id_obra", ""), "pdf": "", "lida": "",
                            "especies_csv": "", "referencia": r.get("citacao", "")}
    return {"achou": False}


def registrar(obra):
    obras = carregar()
    for o in obras:
        if _casa(o, obra.get("doi"), obra.get("titulo")):
            for c in COLUNAS[1:]:
                v = str(obra.get(c) or "").strip()
                if v:
                    o[c] = v
            gravar(obras); return o["id_obra"]
    n = max([int(o["id_obra"][1:]) for o in obras if re.fullmatch(r"B\d+", o["id_obra"])] or [0]) + 1
    novo = {c: str(obra.get(c) or "").strip() for c in COLUNAS}
    novo["id_obra"] = f"B{n:04d}"; novo["lida"] = novo["lida"] or "não"
    if novo["lida"] == "sim" and not novo["lida_em"]:
        novo["lida_em"] = time.strftime("%Y-%m-%d")
    obras.append(novo); gravar(obras)
    return novo["id_obra"]


def catalogar():
    d = os.path.join(pasta(), "pdfs")
    if not os.path.isdir(d):
        return []
    conhecidos = {o["pdf"] for o in carregar()}
    novos = []
    for f in sorted(os.listdir(d)):
        if f.lower().endswith(".pdf") and f not in conhecidos:
            novos.append(registrar({"titulo": os.path.splitext(f)[0], "pdf": f, "lida": "não"}))
    return novos


def main():
    ap = argparse.ArgumentParser(description="Biblioteca de artigos do Quadrante")
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("procurar"); p.add_argument("--doi"); p.add_argument("--titulo")
    sub.add_parser("catalogar")
    r = sub.add_parser("registrar"); r.add_argument("--json", required=True)
    a = ap.parse_args()
    if a.cmd == "procurar":
        out = procurar(a.doi, a.titulo)
    elif a.cmd == "catalogar":
        out = {"novos": catalogar()}
    else:
        out = {"id_obra": registrar(json.load(open(a.json, encoding="utf-8")))}
    sys.stdout.reconfigure(encoding="utf-8")
    print(json.dumps(out, ensure_ascii=False))


if __name__ == "__main__":
    main()
