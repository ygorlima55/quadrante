# Quadrante: dados secundários (literatura da região) e comparação com o campo.
# Desenho: docs/superpowers/specs/2026-11-21-dados-secundarios-design.md (aprovado em 21/11/2026).

# ---- Coordenadas e centro da região

# UTM (WGS84 / SIRGAS 2000, iguais na prática) para graus decimais; fórmulas de Snyder (1987).
utm_para_graus <- function(zona, hemisferio, e, n) {
  a <- 6378137; f <- 1 / 298.257222101; e2 <- f * (2 - f); ep2 <- e2 / (1 - e2); k0 <- 0.9996
  x <- e - 500000; y <- if (toupper(hemisferio) == "S") n - 10000000 else n
  M <- y / k0
  mu <- M / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
  e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
  p1 <- mu + (3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) + (21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
    (151 * e1^3 / 96) * sin(6 * mu) + (1097 * e1^4 / 512) * sin(8 * mu)
  C1 <- ep2 * cos(p1)^2; T1 <- tan(p1)^2; N1 <- a / sqrt(1 - e2 * sin(p1)^2)
  R1 <- a * (1 - e2) / (1 - e2 * sin(p1)^2)^1.5; D <- x / (N1 * k0)
  lat <- p1 - (N1 * tan(p1) / R1) * (D^2 / 2 - (5 + 3 * T1 + 10 * C1 - 4 * C1^2 - 9 * ep2) * D^4 / 24 +
    (61 + 90 * T1 + 298 * C1 + 45 * T1^2 - 252 * ep2 - 3 * C1^2) * D^6 / 720)
  lon <- (D - (1 + 2 * T1 + C1) * D^3 / 6 + (5 - 2 * C1 + 28 * T1 - 3 * C1^2 + 8 * ep2 + 24 * T1^2) * D^5 / 120) / cos(p1)
  c(lat = lat * 180 / pi, lon = (zona - 1) * 6 - 180 + 3 + lon * 180 / pi)
}

# distância em km entre dois pontos em graus (haversine, raio médio 6371 km)
distancia_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  h <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(sqrt(pmin(1, pmax(0, h))))
}

# centro dos pontos amostrais em graus: média das coordenadas legíveis; problema = NA ou o motivo
centro_pontos <- function(pontos) {
  vazio <- function(pr, n = 0L) list(lat = NA_real_, lon = NA_real_, n_pontos = n, problema = pr)
  if (is.null(pontos) || !nrow(pontos) || is.null(pontos$coordenadas)) return(vazio("nenhum ponto com coordenada"))
  gs <- list(); ruins <- character()
  for (i in seq_len(nrow(pontos))) {
    s <- pontos$coordenadas[i]
    if (is.na(s) || !nzchar(trimws(s))) next
    co <- ler_coordenada(s)
    if (is.null(co) || !is.na(co$problema) || is.na(co$crs)) { ruins <- c(ruins, as.character(pontos$ponto[i])); next }
    gs[[length(gs) + 1]] <- if (co$crs == "graus") c(lat = co$y, lon = co$x)
      else utm_para_graus(as.integer(sub(" .*", "", co$crs)), sub(".* ", "", co$crs), co$x, co$y)
  }
  pr <- if (length(ruins)) paste0("coordenada sem zona UTM ou ilegível em ", paste(ruins, collapse = ", ")) else NA_character_
  if (!length(gs)) return(vazio(if (length(ruins)) pr else "nenhum ponto com coordenada"))
  m <- do.call(rbind, gs)
  list(lat = mean(m[, "lat"]), lon = mean(m[, "lon"]), n_pontos = nrow(m), problema = pr)
}

# ---- Planilha padrão (dados_secundarios.xlsx)

# colunas de cada aba e o que são (vira a aba Leia-me)
COLUNAS_SEC <- list(
  Obras = c(id_obra = "código da obra (o mesmo da biblioteca, por exemplo B0001)", referencia = "referência completa", titulo = "título do trabalho",
    ano = "ano de publicação", tipo = "artigo, tese, dissertação, nota, livro ou relatório", doi_url = "DOI ou endereço na internet",
    local_estudo = "onde o estudo foi feito (fazenda, parque, serra, bacia, sítios)", municipios = "municípios da área de estudo, separados por ponto e vírgula", uf = "estado(s)",
    latitude = "latitude do local de estudo mais próximo do centro (graus decimais)", longitude = "longitude (graus decimais)",
    origem_coordenada = "artigo, sede do município ou estimada", distancia_km = "distância do centro até o ponto mais próximo da área de estudo",
    criterio_raio = "sítio dentro do raio (vale mais quando o artigo dá coordenadas), município dentro do raio ou área sobreposta ao raio (só sem coordenada do sítio)", dentro_do_raio = "sim, não ou a conferir (quando não dá para decidir; vazio também vira a conferir)",
    lida = "sim ou não (não = não lida, a lista de espécies não foi conferida)", obra_derivada_de = "id_obra da obra de que esta repete os dados (tese e o artigo dela)",
    metodo = "métodos de amostragem", esforco = "esforço amostral como publicado", observacao = "observações",
    revisado_a_mao = "sim quando você conferiu ou corrigiu a linha (o agente não muda mais)"),
  `Espécies` = c(id_obra = "obra onde a espécie aparece", grupo = "Anuros, Gymnophiona, Testudines, Crocodylia, Squamata",
    familia = "família", nome_publicado = "nome exatamente como na obra", nome_valido = "nome válido atual", nome_comum = "nome popular (se houver fonte)",
    autoria = "autor e ano do nome válido", motivo_da_troca = "sinonímia, nova combinação, grafia...",
    fonte_taxonomica = "Lista da SBH, Frost (ASW) ou Reptile Database, com data", duvida = "por que o nome é duvidoso (sp., cf., aff., distribuição)",
    ameaca_mma = "categoria na lista nacional vigente (MMA; diga a portaria em fonte_taxonomica)", ameaca_iucn = "categoria na IUCN", ameaca_estadual = "categoria na lista do estado",
    endemica_cerrado = "sim ou não", revisado_a_mao = "sim quando você conferiu ou corrigiu a linha"),
  `Nomes do campo` = c(nome_no_campo = "nome como está na planilha de campo", nome_valido = "nome válido atual",
    motivo_da_troca = "por que mudou", fonte_taxonomica = "fonte da revisão", duvida = "dúvida, se houver",
    revisado_a_mao = "sim quando você conferiu ou corrigiu a linha"),
  Busca = c(data = "data da consulta", base = "base consultada (OpenAlex, SciELO, BDTD...)", termos = "termos usados",
    resultados = "número de resultados", triados = "número triado", lidos = "número lido", observacao = "observações")
)
OBRIGATORIAS_SEC <- list(Obras = c("id_obra", "referencia", "dentro_do_raio", "lida"), `Espécies` = c("id_obra", "nome_publicado"))

modelo_secundarios <- function(arquivo) {
  vazio <- function(cols) as.data.frame(setNames(rep(list(character()), length(cols)), names(cols)), check.names = FALSE)
  folhas <- lapply(COLUNAS_SEC, vazio)
  folhas[["Leia-me"]] <- do.call(rbind, lapply(names(COLUNAS_SEC), function(a)
    data.frame(aba = a, coluna = names(COLUNAS_SEC[[a]]), o_que_e = unname(COLUNAS_SEC[[a]]))))
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(folhas, arquivo)
  invisible(arquivo)
}

# nome de coluna ou aba comparável: sem acento, minúsculas, só letras/números com "_"
chave_sec <- function(x) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", sem_acento(norm_txt(x))))
sim_nao <- function(x) { k <- chave_sec(x); ifelse(k == "sim", "sim", ifelse(k == "nao", "não", norm_txt(x))) }

ler_secundarios <- function(arquivo) {
  if (!file.exists(arquivo)) return(NULL)
  tmp <- copiar_planilha(arquivo)
  abas <- readxl::excel_sheets(tmp)
  av <- list(); add <- function(nivel, aba, linha, problema) av[[length(av) + 1]] <<- data.frame(nivel = nivel, aba = aba, linha = as.character(linha), problema = problema)
  le <- function(aba) {
    cols <- names(COLUNAS_SEC[[aba]])
    vazio <- as.data.frame(setNames(rep(list(character()), length(cols)), cols), check.names = FALSE)
    i <- match(chave_sec(aba), chave_sec(abas))
    if (is.na(i)) { if (aba != "Busca") add("atenção", aba, "", paste0("A aba \"", aba, "\" não existe na planilha.")); return(vazio) }
    d <- suppressMessages(readxl::read_excel(tmp, abas[i], col_types = "text"))
    k <- chave_sec(names(d))
    out <- as.data.frame(lapply(setNames(cols, cols), function(cn) { j <- match(cn, k); if (is.na(j)) rep(NA_character_, nrow(d)) else norm_txt(d[[j]]) }), check.names = FALSE)
    out[is.na(out)] <- ""
    out$linha_planilha <- seq_len(nrow(out)) + 1L
    for (cn in setdiff(OBRIGATORIAS_SEC[[aba]], k)) add("erro", abas[i], "", paste0("Falta a coluna \"", cn, "\"."))
    if ("revisado_a_mao" %in% cols && !"revisado_a_mao" %in% k) add("atenção", abas[i], "", "Falta a coluna \"revisado_a_mao\": sem ela, o agente pode mudar linhas que você corrigiu.")
    estranhas <- names(d)[!k %in% cols & nzchar(k) & !grepl("^[.][.][.][0-9]+$", names(d))]
    for (cn in estranhas) { add("atenção", abas[i], "", paste0("Coluna \"", cn, "\" não reconhecida: não entra na análise, e o agente não junta dados nesta planilha enquanto ela existir (renomeie para uma coluna da aba Leia-me ou mova para outra planilha)."))
      desconhecidas <<- c(desconhecidas, paste0(abas[i], ": ", cn)) }
    out[rowSums(out[cols] != "") > 0, , drop = FALSE]
  }
  desconhecidas <- character()
  ob <- le("Obras"); es <- le("Espécies"); nc <- le("Nomes do campo"); bu <- le("Busca")
  ob$revisado_a_mao <- sim_nao(ob$revisado_a_mao); es$revisado_a_mao <- sim_nao(es$revisado_a_mao); nc$revisado_a_mao <- sim_nao(nc$revisado_a_mao)
  ob$dentro_do_raio[chave_sec(ob$dentro_do_raio) %in% c("", "a_conferir")] <- "a conferir"
  for (cn in c("dentro_do_raio", "lida")) {
    ob[[cn]] <- sim_nao(ob[[cn]])
    for (r in which(!ob[[cn]] %in% c("sim", "não", if (cn == "dentro_do_raio") "a conferir"))) add("atenção", "Obras", ob$linha_planilha[r], paste0("Obra ", ob$id_obra[r], ": \"", cn, "\" deve ser sim ou não (está \"", ob[[cn]][r], "\"); a obra fica fora da análise."))
  }
  for (r in which(ob$referencia == "" & ob$doi_url == "")) add("erro", "Obras", ob$linha_planilha[r], paste0("Obra ", ob$id_obra[r], " sem referência e sem DOI/URL: a fonte não é rastreável."))
  for (r in which(!es$id_obra %in% ob$id_obra)) add("erro", "Espécies", es$linha_planilha[r], paste0("A espécie \"", es$nome_publicado[r], "\" cita a obra \"", es$id_obra[r], "\", que não está na aba Obras."))
  es$nome_valido[es$nome_valido == ""] <- es$nome_publicado[es$nome_valido == ""]
  nc$nome_valido[nc$nome_valido == ""] <- nc$nome_no_campo[nc$nome_valido == ""]
  es$endemica_cerrado <- sim_nao(es$endemica_cerrado)
  list(obras = ob, especies = es, nomes_campo = nc, busca = bu, colunas_desconhecidas = desconhecidas,
       avisos = if (length(av)) do.call(rbind, av) else data.frame(nivel = character(), aba = character(), linha = character(), problema = character()))
}

# ---- Análise: descritivos das obras e comparação com o campo

# binômio normalizado ("boana  albopunctata (Spix, 1824)" -> "Boana albopunctata"); sp., cf., aff. ou só gênero -> NA
nome_base <- function(x) {
  vapply(as.character(x), function(s) {
    s <- norm_txt(gsub("\\(.*?\\)", " ", s)); p <- strsplit(s, " ", fixed = TRUE)[[1]]
    if (length(p) < 2 || is.na(p[1])) return(NA_character_)
    ep <- tolower(p[2])
    if (grepl("^(sp|spp|cf|aff|gr|n)\\.?$", ep) || !grepl("^[a-z-]+$", ep)) return(NA_character_)
    paste0(toupper(substr(p[1], 1, 1)), tolower(substring(p[1], 2)), " ", ep)
  }, "", USE.NAMES = FALSE)
}

# herpetofauna: anuros, cecílias, répteis (inclui anfisbenas); grupo vazio é aceito
e_herpeto <- function(g) { g <- ifelse(is.na(g), "", norm_txt(g))
  g == "" | g == "Todas as espécies" | grupo_padrao(g) %in% c("Anuros", "Répteis") | grepl("gymnoph|cecil|caecil|anfisb|amphisb", sem_acento(g)) }

analisar_secundarios <- function(S, registros, regiao) {
  ob <- S$obras; es <- S$especies
  for (cn in names(COLUNAS_SEC$Obras)) if (is.null(ob[[cn]])) ob[[cn]] <- rep("", nrow(ob))   # planilhas antigas ou mínimas
  for (cn in names(COLUNAS_SEC$`Espécies`)) if (is.null(es[[cn]])) es[[cn]] <- rep("", nrow(es))
  usa <- ob$dentro_do_raio == "sim" & ob$lida == "sim"
  ou <- ob[usa, , drop = FALSE]
  raiz <- setNames(ou$id_obra, ou$id_obra)   # obra derivada conta como a obra de origem (segue a cadeia)
  for (k in seq_len(5)) { d <- ou$obra_derivada_de[match(raiz, ou$id_obra)]; troca <- !is.na(d) & d != "" & d %in% ou$id_obra; raiz[troca] <- d[troca] }
  el <- es[es$id_obra %in% ou$id_obra & e_herpeto(es$grupo), , drop = FALSE]
  el$nome <- nome_base(el$nome_valido); el$raiz <- unname(raiz[el$id_obra])
  ok <- el[!is.na(el$nome), , drop = FALSE]
  prim <- function(v) { v <- v[!is.na(v) & v != ""]; if (length(v)) v[1] else "" }
  lit <- if (nrow(ok)) do.call(rbind, lapply(split(ok, ok$nome), function(q) data.frame(nome = q$nome[1], familia = prim(q$familia), grupo = prim(q$grupo),
      autoria = prim(q$autoria), n_obras = length(unique(q$raiz)), obras = paste(unique(q$id_obra), collapse = "; "),
      ameaca_mma = prim(q$ameaca_mma), ameaca_iucn = prim(q$ameaca_iucn), ameaca_estadual = prim(q$ameaca_estadual), endemica_cerrado = prim(q$endemica_cerrado))))
    else data.frame(nome = character(), familia = character(), grupo = character(), autoria = character(), n_obras = integer(), obras = character(),
      ameaca_mma = character(), ameaca_iucn = character(), ameaca_estadual = character(), endemica_cerrado = character())
  lit <- lit[order(-lit$n_obras, lit$nome), , drop = FALSE]; rownames(lit) <- NULL
  # onde cada espécie foi registrada na literatura (local do estudo, municípios, UF, distância)
  lugar <- function(i) { o <- ou[ou$id_obra == i, ][1, ]; dd <- suppressWarnings(as.numeric(gsub(",", ".", o$distancia_km)))
    paste0(i, ": ", paste(c(if (nzchar(o$local_estudo %||% "")) o$local_estudo, if (nzchar(o$municipios)) o$municipios), collapse = ", "),
           if (nzchar(o$uf)) paste0(" (", o$uf, ")"), if (!is.na(dd)) paste0(", ", format(round(dd, 1), decimal.mark = ","), " km")) }
  lit$genero <- sub(" .*", "", lit$nome)
  lit$grupo_pad <- grupo_tax(lit$grupo, lit$familia)
  lit$nome_comum <- vapply(lit$nome, function(n) prim(ok$nome_comum[ok$nome == n]), "")
  lit$locais <- vapply(lit$obras, function(x) paste(vapply(strsplit(x, "; ", fixed = TRUE)[[1]], lugar, ""), collapse = " | "), "", USE.NAMES = FALSE)
  # campo: nome válido pela aba "Nomes do campo"; sp./cf./aff. ficam "a confirmar"
  campo <- data.frame(nome = character(), familia = character(), grupo = character(), confirmar = logical())
  if (!is.null(registros) && nrow(registros)) {   # herpetofauna: aves, mamíferos e outros grupos do campo não entram
    registros <- registros[e_herpeto(registros$grupo_pad), , drop = FALSE]
  }
  if (!is.null(registros) && nrow(registros)) {
    nc <- S$nomes_campo; orig <- unique(norm_txt(registros$especie))
    val <- vapply(orig, function(o) { i <- match(nome_base(o), nome_base(nc$nome_no_campo)); if (is.na(i) || is.na(nome_base(o))) o else nc$nome_valido[i] }, "", USE.NAMES = FALSE)
    nb <- nome_base(val); fam <- registros$familia[match(orig, norm_txt(registros$especie))]; gr <- registros$grupo_pad[match(orig, norm_txt(registros$especie))]
    rot <- ifelse(is.na(nb), paste0(toupper(substr(orig, 1, 1)), substring(orig, 2)), nb)
    campo <- unique(data.frame(nome = rot, familia = ifelse(is.na(fam), "", fam), grupo = ifelse(is.na(gr), "", gr), confirmar = is.na(nb)))
    campo <- campo[!duplicated(campo$nome), , drop = FALSE]
  }
  cv <- campo$nome[!campo$confirmar]
  todos <- union(lit$nome, cv)
  comp <- data.frame(nome = todos, situacao = ifelse(todos %in% lit$nome & todos %in% cv, "literatura e campo", ifelse(todos %in% lit$nome, "só literatura", "só campo")))
  if (any(campo$confirmar)) comp <- rbind(comp, data.frame(nome = campo$nome[campo$confirmar], situacao = "campo a confirmar"))
  comp$familia <- ifelse(comp$nome %in% lit$nome, lit$familia[match(comp$nome, lit$nome)], campo$familia[match(comp$nome, campo$nome)])
  comp$grupo <- ifelse(comp$nome %in% lit$nome, lit$grupo[match(comp$nome, lit$nome)], campo$grupo[match(comp$nome, campo$nome)])
  comp$n_obras <- ifelse(comp$nome %in% lit$nome, lit$n_obras[match(comp$nome, lit$nome)], 0L)
  comp$obras <- ifelse(comp$nome %in% lit$nome, lit$obras[match(comp$nome, lit$nome)], "")
  comp$locais <- ifelse(comp$nome %in% lit$nome, lit$locais[match(comp$nome, lit$nome)], "")
  ordem <- c("literatura e campo", "só literatura", "só campo", "campo a confirmar")
  comp <- comp[order(match(comp$situacao, ordem), comp$familia, comp$nome), , drop = FALSE]; rownames(comp) <- NULL
  cres <- data.frame(situacao = ordem, especies = vapply(ordem, function(s) sum(comp$situacao == s), 0L, USE.NAMES = FALSE))
  ambos <- sum(comp$situacao == "literatura e campo")
  riq_obra <- vapply(ou$id_obra, function(i) length(unique(ok$nome[ok$id_obra == i])), 0L, USE.NAMES = FALSE)
  gp_ok <- grupo_tax(ok$grupo, ok$familia)
  conta_g <- function(i, g) length(unique(ok$nome[ok$id_obra == i & gp_ok == g]))
  ores <- data.frame(id_obra = ou$id_obra, autor_ano = cita_curta(ou$referencia, ou$ano), titulo = ou$titulo, referencia = ou$referencia, ano = ou$ano, tipo = ou$tipo,
    local_estudo = ou$local_estudo, municipios = ou$municipios, uf = ou$uf,
    distancia_km = suppressWarnings(as.numeric(gsub(",", ".", ou$distancia_km))), criterio_raio = ou$criterio_raio,
    n_registros = vapply(ou$id_obra, function(i) sum(el$id_obra == i), 0L, USE.NAMES = FALSE), riqueza = riq_obra,
    anuros = vapply(ou$id_obra, conta_g, 0L, g = "Anuros", USE.NAMES = FALSE), repteis = vapply(ou$id_obra, conta_g, 0L, g = "Répteis", USE.NAMES = FALSE),
    metodo = ou$metodo, esforco = ou$esforco, obra_derivada_de = ou$obra_derivada_de)
  # descritivas por grupo, gênero e família
  grs <- intersect(c("Anuros", "Gymnophiona", "Répteis"), unique(lit$grupo_pad))
  por_grupo <- data.frame(grupo = grs, especies = vapply(grs, function(g) sum(lit$grupo_pad == g), 0L),
    generos = vapply(grs, function(g) length(unique(lit$genero[lit$grupo_pad == g])), 0L),
    familias = vapply(grs, function(g) length(unique(lit$familia[lit$grupo_pad == g & lit$familia != ""])), 0L),
    obras = vapply(grs, function(g) length(unique(ok$id_obra[gp_ok == g])), 0L), row.names = NULL)
  generos <- if (nrow(lit)) do.call(rbind, lapply(split(lit, lit$genero), function(q) data.frame(grupo = q$grupo_pad[1], familia = prim(q$familia), genero = q$genero[1],
      n_especies = nrow(q), especies = paste(sort(q$nome), collapse = "; "), n_obras = length(unique(ok$raiz[ok$nome %in% q$nome]))))) else
    data.frame(grupo = character(), familia = character(), genero = character(), n_especies = integer(), especies = character(), n_obras = integer())
  generos <- generos[order(generos$grupo, generos$familia, generos$genero), , drop = FALSE]; rownames(generos) <- NULL
  fl <- lit[lit$familia != "", , drop = FALSE]
  familias <- if (nrow(fl)) do.call(rbind, lapply(split(fl, paste(fl$grupo_pad, fl$familia)), function(q) data.frame(grupo = q$grupo_pad[1], familia = q$familia[1],
      n_generos = length(unique(q$genero)), n_especies = nrow(q)))) else data.frame(grupo = character(), familia = character(), n_generos = integer(), n_especies = integer())
  familias <- familias[order(familias$grupo, -familias$n_especies, familias$familia), , drop = FALSE]; rownames(familias) <- NULL
  av <- data.frame(nivel = character(), aba = character(), linha = character(), problema = character())
  rk <- suppressWarnings(as.numeric(regiao$raio_km %||% NA)); dk <- suppressWarnings(as.numeric(gsub(",", ".", ob$distancia_km)))
  if (length(rk) == 1 && !is.na(rk)) for (r in which(ob$dentro_do_raio == "sim" & !is.na(dk) & dk > rk))
    av <- rbind(av, data.frame(nivel = "atenção", aba = "Obras", linha = as.character(ob$linha_planilha[r] %||% ""),
      problema = paste0("Obra ", ob$id_obra[r], " está a ", format(dk[r]), " km, mais longe que o raio salvo (", format(rk), " km), mas está marcada dentro do raio. Confira dentro_do_raio ou rode o agente de novo.")))
  conta <- function(v, nome) { v[is.na(v) | v == ""] <- "não informado"; t <- table(v)
    setNames(data.frame(as.character(names(t)), as.integer(t)), c(nome, "obras")) }
  list(regiao = regiao, avisos = av, obras_resumo = ores, por_ano = conta(ou$ano, "ano"), por_tipo = conta(ou$tipo, "tipo"), por_criterio = conta(ou$criterio_raio, "criterio"),
       especies = lit, mais_citadas = head(lit[, c("nome", "familia", "n_obras")], 20), comparacao = comp, comparacao_resumo = cres,
       por_grupo = por_grupo, generos = generos, familias = familias,
       nao_lidas = ob[ob$lida != "sim" & ob$dentro_do_raio == "sim", c("id_obra", "referencia", "doi_url"), drop = FALSE],
       fora_do_raio = ob[ob$dentro_do_raio == "não", c("id_obra", "referencia", "distancia_km"), drop = FALSE],
       a_conferir = ob[!ob$dentro_do_raio %in% c("sim", "não"), c("id_obra", "referencia", "distancia_km", "observacao"), drop = FALSE],
       duvidas = el[is.na(el$nome) | el$duvida != "", c("id_obra", "nome_publicado", "duvida"), drop = FALSE],
       resumo = list(obras_usadas = nrow(ou), obras_independentes = length(unique(raiz)), nao_lidas = sum(ob$lida != "sim" & ob$dentro_do_raio == "sim"),
                     fora_do_raio = sum(ob$dentro_do_raio == "não"), obras_a_conferir = sum(!ob$dentro_do_raio %in% c("sim", "não")), riqueza_secundaria = nrow(lit), riqueza_campo = length(cv), em_ambos = ambos,
                     a_confirmar = sum(campo$confirmar), pct_registrada_campo = if (nrow(lit)) 100 * ambos / nrow(lit) else NA_real_,
                     riqueza_anuros = sum(lit$grupo_pad == "Anuros"), riqueza_repteis = sum(lit$grupo_pad == "Répteis"),
                     generos = length(unique(lit$genero)), familias = length(unique(lit$familia[lit$familia != ""]))))
}

# grupo padronizado de uma espécie da literatura: Anuros, Gymnophiona ou Répteis (pelo grupo escrito ou, sem ele, pela família)
FAMILIAS_REPTEIS <- c("Chelidae", "Kinosternidae", "Podocnemididae", "Testudinidae", "Emydidae", "Geoemydidae", "Alligatoridae", "Crocodylidae",
  "Amphisbaenidae", "Colubridae", "Dipsadidae", "Elapidae", "Viperidae", "Boidae", "Typhlopidae", "Leptotyphlopidae", "Anomalepididae", "Aniliidae",
  "Tropiduridae", "Teiidae", "Gymnophthalmidae", "Scincidae", "Mabuyidae", "Gekkonidae", "Phyllodactylidae", "Sphaerodactylidae", "Polychrotidae",
  "Dactyloidae", "Leiosauridae", "Iguanidae", "Anguidae", "Diploglossidae")
grupo_tax <- function(grupo, familia) {
  g <- ifelse(is.na(grupo), "", grupo); f <- ifelse(is.na(familia), "", familia)
  out <- ifelse(grepl("gymnoph|cecil|caecil|siphonops|typhlonect", sem_acento(paste(g, f))), "Gymnophiona", grupo_padrao(g))
  out[out %in% c("Todas as espécies", "") | !out %in% c("Anuros", "Répteis", "Gymnophiona")] <- ifelse(f %in% FAMILIAS_REPTEIS, "Répteis", "Anuros")[out %in% c("Todas as espécies", "") | !out %in% c("Anuros", "Répteis", "Gymnophiona")]
  out
}

# "Signorelli, L. 2014. Título..." -> "Signorelli 2014"; com dois autores "A & B 2007"; mais de dois "A et al. 2014"
cita_curta <- function(ref, ano) {
  vapply(seq_along(ref), function(i) {
    r <- norm_txt(ref[i]); a <- if (nzchar(ano[i] %||% "")) ano[i] else sub(".*?((19|20)[0-9]{2}).*", "\\1", r)
    autores <- sub("[.,]? *\\(?(19|20)[0-9]{2}.*$", "", r)
    sobren <- regmatches(autores, gregexpr("(^|[,&;] *|e )([A-ZÀ-Ý][A-Za-zÀ-ÿ'-]+)(?=,)", autores, perl = TRUE))[[1]]
    sobren <- trimws(gsub("^[,&;e ]+", "", sobren))
    nome <- if (!length(sobren)) sub("[ ,].*", "", r) else if (grepl("et al", autores)) paste(sobren[1], "et al.") else if (length(sobren) == 1) sobren[1]
            else if (length(sobren) == 2) paste(sobren[1], "&", sobren[2]) else paste(sobren[1], "et al.")
    paste(nome, a)
  }, "", USE.NAMES = FALSE)
}

# ---- Tabela de espécies no estilo do relatório: ordem > família > espécie, uma coluna por obra (x), total, campo e status
ORDENS_REPTEIS <- list(Testudines = c("Chelidae", "Kinosternidae", "Podocnemididae", "Testudinidae", "Emydidae", "Geoemydidae"),
                       Crocodylia = c("Alligatoridae", "Crocodylidae"))
ordem_tax <- function(grupo_pad, familia) {
  ifelse(grupo_pad == "Anuros", "Anuros", ifelse(grupo_pad == "Gymnophiona", "Gymnophiona",
    ifelse(familia %in% ORDENS_REPTEIS$Testudines, "Testudines", ifelse(familia %in% ORDENS_REPTEIS$Crocodylia, "Crocodylia", "Squamata"))))
}

tabela_especies_sec <- function(A) {
  ob <- A$obras_resumo; lit <- A$especies; cp <- A$comparacao
  so_campo <- cp[cp$situacao == "só campo", , drop = FALSE]
  base <- rbind(
    data.frame(texto = lit$nome, autoria = lit$autoria, nome_comum = lit$nome_comum %||% rep("", nrow(lit)), familia = lit$familia, grupo_pad = lit$grupo_pad,
               obras = lit$obras, total = lit$n_obras, endemica = ifelse(lit$endemica_cerrado == "sim", "CE", ""),
               mma = lit$ameaca_mma, iucn = lit$ameaca_iucn, estadual = lit$ameaca_estadual),
    if (nrow(so_campo)) data.frame(texto = so_campo$nome, autoria = "", nome_comum = "", familia = so_campo$familia, grupo_pad = grupo_tax(so_campo$grupo, so_campo$familia),
               obras = "", total = 0L, endemica = "", mma = "", iucn = "", estadual = ""))
  campo_ok <- cp$nome[cp$situacao %in% c("literatura e campo", "só campo")]
  base$campo <- ifelse(base$texto %in% campo_ok, "x", "")
  base$familia[is.na(base$familia) | base$familia == ""] <- "Família não informada"
  base$ordem <- ordem_tax(base$grupo_pad, base$familia)
  for (i in ob$id_obra) base[[i]] <- ifelse(vapply(strsplit(base$obras, "; ", fixed = TRUE), function(v) i %in% v, TRUE), "x", "")
  ordens <- intersect(c("Anuros", "Gymnophiona", "Testudines", "Crocodylia", "Squamata"), unique(base$ordem))
  cols <- c("texto", "autoria", "nome_comum", ob$id_obra, "total", "campo", "endemica", "mma", "iucn", "estadual")
  vazio <- function(tipo, txt) { d <- as.data.frame(setNames(rep(list(""), length(cols)), cols), check.names = FALSE); d$texto <- txt; d$total <- NA; cbind(tipo = tipo, d) }
  linhas <- list()
  for (o in ordens) {
    linhas[[length(linhas) + 1]] <- vazio("ordem", o)
    bo <- base[base$ordem == o, , drop = FALSE]
    for (f in sort(unique(bo$familia))) {
      linhas[[length(linhas) + 1]] <- vazio("familia", f)
      bf <- bo[bo$familia == f, , drop = FALSE]; bf <- bf[order(bf$texto), , drop = FALSE]
      linhas[[length(linhas) + 1]] <- cbind(tipo = "especie", bf[, cols, drop = FALSE])
    }
  }
  L <- if (length(linhas)) do.call(rbind, linhas) else cbind(tipo = character(), vazio("x", "")[0, -1])
  rownames(L) <- NULL
  list(obras = ob[, c("id_obra", "autor_ano", "referencia", "local_estudo", "municipios", "uf", "distancia_km")], linhas = L)
}

html_tabela_sec <- function(T, titulo = "Espécies registradas na literatura da região") {
  esc <- function(x) { x <- ifelse(is.na(x), "", as.character(x)); x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
  ob <- T$obras; L <- T$linhas; no <- nrow(ob); ncol_tot <- 3 + no + 6
  verde <- "#c6dfb5"; verde2 <- "#e4f0da"; borda <- "#9bbb85"
  td <- function(x, extra = "") paste0("<td style=\"text-align:center;padding:2px 6px;", extra, "\">", x, "</td>")
  cab1 <- paste0("<tr style=\"background:", verde, ";font-weight:bold\">",
    "<th rowspan=\"2\" style=\"padding:4px 8px\">Ordem/ Família/ Espécie</th><th rowspan=\"2\" style=\"padding:4px 8px\">Nome comum</th>",
    if (no) paste0("<th colspan=\"", no, "\">Estudos (literatura)</th>"), "<th rowspan=\"2\">Total</th><th rowspan=\"2\">Campo</th><th rowspan=\"2\">End.</th>",
    "<th colspan=\"3\">Status</th></tr>")
  cab2 <- paste0("<tr style=\"background:", verde, ";font-weight:bold\">", paste0("<th style=\"padding:2px 6px\">", esc(ob$id_obra), "</th>", collapse = ""),
    "<th>MMA</th><th>IUCN</th><th>Estadual</th></tr>")
  corpo <- vapply(seq_len(nrow(L)), function(i) {
    r <- L[i, ]
    if (r$tipo == "ordem") return(paste0("<tr style=\"background:", verde, ";font-weight:bold\"><td colspan=\"", ncol_tot, "\" style=\"padding:3px 8px\">", esc(r$texto), "</td></tr>"))
    if (r$tipo == "familia") return(paste0("<tr style=\"background:", verde2, ";font-weight:bold\"><td colspan=\"", ncol_tot, "\" style=\"text-align:center;padding:3px 8px\">", esc(r$texto), "</td></tr>"))
    traco <- function(v) ifelse(is.na(v) | v == "", "-", esc(v))
    paste0("<tr>", "<td style=\"padding:2px 8px;text-align:center;white-space:nowrap\"><i>", esc(r$texto), "</i>", if (nzchar(r$autoria)) paste0(" ", esc(r$autoria)), "</td>",
      td(traco(r$nome_comum)), paste(vapply(ob$id_obra, function(k) td(traco(r[[k]])), ""), collapse = ""),
      td(r$total), td(traco(r$campo)), td(traco(r$endemica)), td(traco(r$mma)), td(traco(r$iucn)), td(traco(r$estadual)), "</tr>")
  }, "")
  leg <- if (no) paste0("<p style=\"margin:8px 0 2px;font-weight:bold\">Estudos de onde vieram as informações</p><ul style=\"margin:0;padding-left:18px\">",
    paste0("<li><b>", esc(ob$id_obra), "</b> (", esc(ob$autor_ano), "): ", esc(ob$referencia), " <i>Local:</i> ",
      esc(paste(c(), collapse = "")), esc(ifelse(nzchar(ob$local_estudo), paste0(ob$local_estudo, ", "), "")), esc(ob$municipios),
      ifelse(nzchar(ob$uf), paste0(" (", esc(ob$uf), ")"), ""), ifelse(is.na(ob$distancia_km), "", paste0(", ", format(round(ob$distancia_km, 1), decimal.mark = ","), " km do centro")), ".</li>", collapse = ""),
    "</ul>") else ""
  nota <- "<p style=\"margin:6px 0 0\">x = citada no estudo; Total = número de estudos que citam a espécie; Campo = registrada no levantamento de campo; End. = CE, endêmica do Cerrado; MMA = lista nacional vigente; - = não consta ou não verificado.</p>"
  out <- paste0("<div style=\"font-family:'Times New Roman',serif;font-size:12px;color:#000\"><p style=\"font-weight:bold;margin:0 0 6px\">", esc(titulo), "</p>",
    "<table style=\"border-collapse:collapse;border-top:1.5px solid ", borda, ";border-bottom:1.5px solid ", borda, "\">", cab1, cab2, paste(corpo, collapse = ""), "</table>", nota, leg, "</div>")
  # estilo fixo em cada célula: a tabela fica igual no painel, no arquivo e colada no Word ou no Excel
  fixo <- "font-family:'Times New Roman',serif;font-size:12px;color:#000;text-transform:none;letter-spacing:normal;font-variant:normal;border:none;"
  out <- gsub("<th style=\"", paste0("<th style=\"", fixo, "text-align:center;"), out, fixed = TRUE)
  out <- gsub("<th>", paste0("<th style=\"", fixo, "text-align:center\">"), out, fixed = TRUE)
  out <- gsub("<th (rowspan|colspan)", paste0("<th style=\"", fixo, "text-align:center;padding:2px 6px\" \\1"), out)
  gsub("<td style=\"", paste0("<td style=\"", fixo), out, fixed = TRUE)
}

# ---- Juntar o que o agente trouxe (CSVs com ; em UTF-8) na planilha do levantamento
# Linha com revisado_a_mao = sim nunca muda; obra casa por id_obra, DOI ou referência; espécie por obra + nome publicado.
juntar_secundarios <- function(arquivo, pasta_csv) {
  if (!file.exists(arquivo)) modelo_secundarios(arquivo)
  S <- ler_secundarios(arquivo)
  if (length(S$colunas_desconhecidas)) stop("A planilha tem colunas que o Quadrante não reconhece (", paste(S$colunas_desconhecidas, collapse = "; "),
    "). Renomeie para uma coluna da aba Leia-me ou mova para outra planilha antes de juntar: regravar a planilha apagaria essas colunas.")
  le_csv <- function(f, aba) {
    cols <- names(COLUNAS_SEC[[aba]]); arq <- file.path(pasta_csv, f)
    if (!file.exists(arq)) return(NULL)
    d <- utils::read.csv(arq, sep = ";", colClasses = "character", encoding = "UTF-8", check.names = FALSE, na.strings = character())
    k <- chave_sec(names(d))
    out <- as.data.frame(lapply(setNames(cols, cols), function(cn) { j <- match(cn, k); if (is.na(j)) rep("", nrow(d)) else norm_txt(d[[j]]) }), check.names = FALSE)
    out[is.na(out)] <- ""; out
  }
  sem_linha <- function(d) d[setdiff(names(d), "linha_planilha")]
  nd <- function(x) gsub("^(https?://(dx[.])?doi[.]org/|doi:\\s*)", "", tolower(trimws(x)))
  conta <- list(obras_novas = 0L, obras_atualizadas = 0L, especies_novas = 0L, especies_atualizadas = 0L, preservadas = 0L)
  log <- character()
  funde <- function(velho, novo, chave, rotulo, n_novo, n_atu) {
    velho <- sem_linha(velho)
    if (is.null(novo) || !nrow(novo)) return(velho)
    for (i in seq_len(nrow(novo))) {
      j <- chave(velho, novo[i, ])
      if (!length(j)) { velho <- rbind(velho, novo[i, names(velho)]); conta[[n_novo]] <<- conta[[n_novo]] + 1L; log <<- c(log, paste("novo:", rotulo(novo[i, ]))); next }
      j <- j[1]
      if (identical(velho$revisado_a_mao[j], "sim")) { conta$preservadas <<- conta$preservadas + 1L; log <<- c(log, paste("preservado (revisado à mão):", rotulo(velho[j, ]))); next }
      mudou <- FALSE
      for (cn in setdiff(names(velho), c("revisado_a_mao", "id_obra"))) if (nzchar(novo[[cn]][i]) && !identical(velho[[cn]][j], novo[[cn]][i])) { velho[[cn]][j] <- novo[[cn]][i]; mudou <- TRUE }
      if (mudou) { conta[[n_atu]] <<- conta[[n_atu]] + 1L; log <<- c(log, paste("atualizado:", rotulo(velho[j, ]))) }
    }
    velho
  }
  casa_obra <- function(v, n) which(v$id_obra == n$id_obra |
      (nzchar(n$doi_url) & nd(v$doi_url) == nd(n$doi_url)) | (nzchar(n$referencia) & chave_sec(v$referencia) == chave_sec(n$referencia)))
  no <- le_csv("obras.csv", "Obras"); ne <- le_csv("especies.csv", "Espécies")
  # obra que já existe com outro código (casou por DOI ou referência): as espécies do agente vão para o código da planilha
  if (!is.null(no) && !is.null(ne)) for (i in seq_len(nrow(no))) { j <- casa_obra(S$obras, no[i, ])
    if (length(j) && S$obras$id_obra[j[1]] != no$id_obra[i]) { ne$id_obra[ne$id_obra == no$id_obra[i]] <- S$obras$id_obra[j[1]]
      log <- c(log, paste0("código ", no$id_obra[i], " do agente = obra ", S$obras$id_obra[j[1]], " da planilha (mantido ", S$obras$id_obra[j[1]], ")")) } }
  ob <- funde(S$obras, no, casa_obra, function(x) paste(x$id_obra, x$referencia), "obras_novas", "obras_atualizadas")
  es <- funde(S$especies, ne, function(v, n) which(v$id_obra == n$id_obra & chave_sec(v$nome_publicado) == chave_sec(n$nome_publicado)),
    function(x) paste(x$id_obra, x$nome_publicado), "especies_novas", "especies_atualizadas")
  nc <- funde(S$nomes_campo, le_csv("nomes_campo.csv", "Nomes do campo"), function(v, n) which(chave_sec(v$nome_no_campo) == chave_sec(n$nome_no_campo)),
    function(x) x$nome_no_campo, "especies_novas", "especies_atualizadas")
  bu <- sem_linha(S$busca); nb <- le_csv("busca.csv", "Busca"); if (!is.null(nb)) bu <- rbind(bu, nb)
  leia <- do.call(rbind, lapply(names(COLUNAS_SEC), function(a) data.frame(aba = a, coluna = names(COLUNAS_SEC[[a]]), o_que_e = unname(COLUNAS_SEC[[a]]))))
  writexl::write_xlsx(list(Obras = ob, `Espécies` = es, `Nomes do campo` = nc, Busca = bu, `Leia-me` = leia), arquivo)
  writeLines(c(paste("Junção de", format(Sys.time(), "%d/%m/%Y %H:%M")), paste0(names(conta), ": ", unlist(conta)), "", log),
             file.path(dirname(arquivo), format(Sys.time(), "mudancas_%Y-%m-%d_%H%M%S.txt")), useBytes = TRUE)
  invisible(conta)
}

# ---- Figuras (pasta figuras/, prefixo sec_) e tabelas

figuras_secundarios <- function(A, saida, e) {
  suppressPackageStartupMessages(library(ggplot2)); registrar_fontes(e)
  pasta <- file.path(saida, "figuras"); dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(pasta, "^sec_.*\\.(png|jpg)$", full.names = TRUE))
  k <- cores_tema(e); pal <- e$paleta; tp <- if (.Platform$OS.type == "windows") "windows" else "cairo"
  tg <- tema_gg(e) + ggplot2::theme(legend.position = "none")
  inteiros <- function(l) { b <- pretty(c(0, l[2])); b[b == round(b) & b >= 0] }
  rot <- function(x) ggplot2::geom_text(ggplot2::aes(label = x), hjust = -0.25, family = e$familia, size = e$fonte / 3.2, colour = k$tinta)
  salva <- function(nome, p, h = e$altura) {
    arq <- file.path(pasta, paste0(nome, ".png"))
    ggplot2::ggsave(arq, p, width = e$largura, height = h, dpi = e$dpi, bg = k$fundo, device = "png", type = tp)
    if ((e$dpi_jpeg %||% 0) > 0) ggplot2::ggsave(sub("\\.png$", ".jpg", arq), p, width = e$largura, height = h, dpi = e$dpi_jpeg, bg = k$fundo, device = "jpeg", type = tp, quality = 100)
    basename(arq)
  }
  barras <- function(d, x, y, xlab, ylab, cor) ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_col(fill = cor, width = .7) + rot(d[[y]]) + ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(breaks = inteiros, expand = ggplot2::expansion(mult = c(0, .12))) + ggplot2::labs(x = xlab, y = ylab) + tg
  alt <- function(n) min(e$altura, max(2, .45 * n + 1.3))   # barras finas quando há poucas categorias
  out <- list()
  if (nrow(A$por_ano)) {
    d <- A$por_ano; d$ano <- factor(d$ano, levels = rev(sort(unique(d$ano))))
    out$sec_obras_ano <- salva("sec_obras_ano", barras(d, "ano", "obras", "Ano de publicação", "Obras", pal[1]), alt(nrow(d)))
  }
  if (nrow(A$por_criterio)) {
    d <- A$por_criterio; d$criterio <- factor(d$criterio, levels = rev(d$criterio))
    out$sec_criterio <- salva("sec_criterio", barras(d, "criterio", "obras", NULL, "Obras", pal[3]), alt(nrow(d)))
  }
  if (nrow(A$mais_citadas)) {
    d <- A$mais_citadas; d$nome <- factor(d$nome, levels = rev(d$nome))
    p <- barras(d, "nome", "n_obras", NULL, "Obras que citam a espécie", pal[1]) + ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic"))
    out$sec_mais_citadas <- salva("sec_mais_citadas", p, max(e$altura, .25 * nrow(d) + 1))
  }
  if (nrow(A$familias %||% data.frame())) {   # espécies por família, anuros e répteis em cores diferentes
    d <- A$familias; d$rot <- paste0(d$familia, "  "); d$rot <- factor(d$rot, levels = rev(unique(d$rot)))
    cg <- setNames(pal[c(1, 4, 3)], c("Anuros", "Gymnophiona", "Répteis"))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = rot, y = n_especies, fill = grupo)) + ggplot2::geom_col(width = .7) + rot(d$n_especies) +
      ggplot2::scale_fill_manual(values = cg, name = NULL) + ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(breaks = inteiros, expand = ggplot2::expansion(mult = c(0, .12))) + ggplot2::labs(x = NULL, y = "Espécies na literatura") +
      tema_gg(e) + ggplot2::theme(legend.position = "top")
    out$sec_familias <- salva("sec_familias", p, max(alt(nrow(d)), min(9, .3 * nrow(d) + 1.4)))
  }
  if (sum(A$comparacao_resumo$especies)) {
    d <- A$comparacao_resumo; d$situacao <- factor(d$situacao, levels = rev(d$situacao))
    p <- ggplot2::ggplot(d, ggplot2::aes(x = situacao, y = especies, fill = situacao)) + ggplot2::geom_col(width = .7) + rot(d$especies) +
      ggplot2::scale_fill_manual(values = setNames(pal[c(1, 3, 2, 4)], rev(levels(d$situacao)))) + ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(breaks = inteiros, expand = ggplot2::expansion(mult = c(0, .12))) + ggplot2::labs(x = NULL, y = "Espécies") + tg
    out$sec_comparacao <- salva("sec_comparacao", p, alt(nrow(d)))
  }
  out
}

exportar_secundarios <- function(A, saida) {
  pasta <- file.path(saida, "tabelas"); dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  r <- A$resumo
  res <- data.frame(medida = c("Obras usadas", "Obras independentes (sem repetir teses e artigos derivados)", "Obras não lidas (dentro do raio)", "Obras fora do raio", "Obras a conferir (raio indefinido)",
                               "Riqueza dos dados secundários", "Anuros na literatura", "Répteis na literatura", "Gêneros na literatura", "Famílias na literatura", "Riqueza do campo (espécies confirmadas)", "Espécies nas duas fontes",
                               "Registros de campo a confirmar (sp., cf., aff.)", "% da riqueza secundária registrada em campo"),
                    valor = c(r$obras_usadas, r$obras_independentes, r$nao_lidas, r$fora_do_raio, r$obras_a_conferir, r$riqueza_secundaria, r$riqueza_anuros %||% NA, r$riqueza_repteis %||% NA, r$generos %||% NA, r$familias %||% NA, r$riqueza_campo, r$em_ambos, r$a_confirmar,
                              if (is.na(r$pct_registrada_campo)) NA else round(r$pct_registrada_campo, 1)))
  if (!is.null(A$regiao$raio_km)) res <- rbind(data.frame(medida = c("Centro (latitude)", "Centro (longitude)", "Raio (km)"), valor = c(A$regiao$lat, A$regiao$lon, A$regiao$raio_km)), res)
  writexl::write_xlsx(list("Obras usadas" = A$obras_resumo, "Espécies" = A$especies, "Comparação com o campo" = A$comparacao, "Resumo" = res,
                           "Não lidas" = A$nao_lidas, "Fora do raio" = A$fora_do_raio, "A conferir" = A$a_conferir,
                           "Por grupo" = A$por_grupo, "Gêneros" = A$generos, "Famílias" = A$familias), file.path(pasta, "10_dados_secundarios.xlsx"))
  # planilha 09 (relatório de fauna de cada grupo): acrescenta as duas abas, trocando as de uma rodada anterior
  for (f in list.files(pasta, "^09_(Anuros|Repteis)_relatorio_fauna\\.xlsx$", full.names = TRUE, ignore.case = TRUE)) {
    tryCatch({
      abas <- setdiff(readxl::excel_sheets(f), c("Dados secundários", "Literatura x campo"))
      folhas <- lapply(setNames(abas, abas), function(a) suppressMessages(readxl::read_excel(f, a)))
      folhas[["Dados secundários"]] <- A$especies; folhas[["Literatura x campo"]] <- A$comparacao
      writexl::write_xlsx(folhas, f)
    }, error = function(x) message("planilha ", basename(f), ": ", conditionMessage(x)))
  }
  T <- tabela_especies_sec(A)
  writeLines(c("<!doctype html><html lang=\"pt-BR\"><head><meta charset=\"utf-8\"><title>Tabela de espécies (literatura)</title></head><body>",
               html_tabela_sec(T), "</body></html>"), file.path(pasta, "11_tabela_especies_literatura.html"), useBytes = TRUE)
  invisible(file.path(pasta, "10_dados_secundarios.xlsx"))
}
