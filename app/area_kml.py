"""Lê o KML ou KMZ da área do levantamento (limite da fazenda, ponto da sede ou pontos) e acha o centro.

ler_area(bytes, nome_arquivo) -> {"nome", "tipo": "polígono"|"linha"|"ponto", "n_feicoes", "lat", "lon", "area_ha"}
O centro de polígonos é o centroide ponderado pela área; de linhas e pontos, a média dos vértices.
Recusa (ValueError com o motivo em português) o que não for KML, não tiver coordenadas ou cair fora do Brasil.
Só biblioteca padrão do Python.
"""
import io, math, re, zipfile
import xml.etree.ElementTree as ET


def _sem_ns(tag):
    return tag.rsplit("}", 1)[-1]


def _coords(texto):
    out = []
    for parte in (texto or "").split():
        v = parte.split(",")
        if len(v) >= 2:
            try:
                out.append((float(v[0]), float(v[1])))
            except ValueError:
                pass
    return out


def _centroide(anel):
    """Centroide e área (em graus², sinal ignorado) de um anel lon/lat."""
    a = cx = cy = 0.0
    for (x1, y1), (x2, y2) in zip(anel, anel[1:] + anel[:1]):
        f = x1 * y2 - x2 * y1
        a += f; cx += (x1 + x2) * f; cy += (y1 + y2) * f
    if abs(a) < 1e-15:
        return sum(p[0] for p in anel) / len(anel), sum(p[1] for p in anel) / len(anel), 0.0
    return cx / (3 * a), cy / (3 * a), abs(a) / 2


def ler_area(dados, nome_arquivo=""):
    if nome_arquivo.lower().endswith(".kmz") or dados[:2] == b"PK":
        try:
            with zipfile.ZipFile(io.BytesIO(dados)) as z:
                kmls = [n for n in z.namelist() if n.lower().endswith(".kml")]
                if not kmls:
                    raise ValueError("O KMZ não tem nenhum arquivo .kml dentro.")
                dados = z.read("doc.kml" if "doc.kml" in kmls else kmls[0])
        except zipfile.BadZipFile:
            raise ValueError("O arquivo KMZ está corrompido.")
    try:
        raiz = ET.fromstring(dados)
    except ET.ParseError:
        raise ValueError("O arquivo não é um KML válido (exporte de novo pelo Google Earth ou QGIS).")
    nome = ""
    poligonos, linhas, pontos = [], [], []
    for el in raiz.iter():
        t = _sem_ns(el.tag)
        if t == "name" and not nome and (el.text or "").strip():
            nome = el.text.strip()
        elif t == "Polygon":
            for sub in el.iter():
                if _sem_ns(sub.tag) == "outerBoundaryIs":
                    for c in sub.iter():
                        if _sem_ns(c.tag) == "coordinates":
                            anel = _coords(c.text)
                            if len(anel) >= 3:
                                poligonos.append(anel)
        elif t == "LineString":
            for c in el.iter():
                if _sem_ns(c.tag) == "coordinates" and _coords(c.text):
                    linhas.append(_coords(c.text))
        elif t == "Point":
            for c in el.iter():
                if _sem_ns(c.tag) == "coordinates" and _coords(c.text):
                    pontos.append(_coords(c.text)[0])
    if poligonos:
        tipo, n = "polígono", len(poligonos)
        cs = [_centroide(a) for a in poligonos]
        peso = sum(c[2] for c in cs)
        if peso > 0:
            lon = sum(c[0] * c[2] for c in cs) / peso; lat = sum(c[1] * c[2] for c in cs) / peso
        else:
            lon = sum(c[0] for c in cs) / n; lat = sum(c[1] for c in cs) / n
        km_lat = 111.32; km_lon = 111.32 * math.cos(math.radians(lat))
        area_ha = peso * km_lat * km_lon * 100
    elif linhas or pontos:
        tipo = "linha" if linhas else "ponto"; n = len(linhas) + len(pontos)
        todos = [p for l in linhas for p in l] + pontos
        lon = sum(p[0] for p in todos) / len(todos); lat = sum(p[1] for p in todos) / len(todos)
        area_ha = None
    else:
        raise ValueError("Não achei coordenadas no KML (precisa de um polígono, linha ou ponto).")
    if not (-35 <= lat <= 7 and -75 <= lon <= -28):
        raise ValueError(f"O centro do KML ({lat:.4f}, {lon:.4f}) fica fora do Brasil. Confira se o arquivo está em graus (WGS84/SIRGAS).")
    return {"nome": re.sub(r"\s+", " ", nome)[:120], "tipo": tipo, "n_feicoes": n, "lat": round(lat, 6), "lon": round(lon, 6),
            "area_ha": round(area_ha, 1) if area_ha else None}
