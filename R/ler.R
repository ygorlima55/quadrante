# Quadrante: leitura da planilha de campo e conversão para tabela longa.
# Formato aceito: aba de registros (um registro por linha) com colunas Ponto, Espécie e
# contagem (Total ou Abundância). Família, Grupo/Classe, VO, AV e Campanha são opcionais.

copiar_planilha <- function(arquivo) {
  # Copia para a pasta temporária: o R não abre arquivo bloqueado pelo Excel nem
  # (no unzip) caminho com caracteres como "°". A cópia pelo Windows contorna os dois.
  tmp <- tempfile(fileext = ".xlsx")
  ok <- suppressWarnings(file.copy(arquivo, tmp))
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    Sys.setenv(HERP_SRC = arquivo, HERP_DST = tmp)
    system2("powershell", c("-NoProfile", "-Command",
      shQuote("Copy-Item -LiteralPath $env:HERP_SRC -Destination $env:HERP_DST -Force")))
  }
  if (!file.exists(tmp)) stop("Não consegui abrir a planilha: ", arquivo)
  tmp
}

norm_txt <- function(x) {
  x <- gsub(" ", " ", as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

sem_acento <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT"))

acha_coluna <- function(nomes, padroes) {
  n <- sem_acento(norm_txt(nomes))
  for (p in padroes) {
    i <- which(grepl(p, n))
    if (length(i)) return(i[1])
  }
  NA_integer_
}

# "Ponto 05", "P5", "5", "Ponto 5" -> "P5"
norm_ponto <- function(x) {
  x <- norm_txt(x)
  numerico <- grepl("^(?:p(?:onto)?\\s*[-_]?\\s*)?[0-9]+$", x, ignore.case = TRUE, perl = TRUE)
  x[numerico] <- paste0("P", sub("^0+(?=[0-9])", "", sub("^[^0-9]*", "", x[numerico]), perl = TRUE))
  x
}

# binômio sem autoria: "Rhinella diptycha (Cope, 1862)" -> "Rhinella diptycha"
binomio <- function(x) {
  x <- norm_txt(x)
  w <- strsplit(x, " ")
  vapply(w, function(v) {
    if (length(v) < 2) return(v[1] %||% "")
    if (v[2] %in% c("sp.", "sp", "spp.", "cf.", "aff.", "gr.")) {
      if (v[2] %in% c("cf.", "aff.", "gr.") && length(v) >= 3) return(paste(v[1:3], collapse = " "))
      if (length(v) >= 3 && grepl("^[0-9]+[A-Za-z]?$", v[3])) return(paste(v[1], "sp.", v[3]))
      return(paste(v[1], "sp."))
    }
    paste(v[1:2], collapse = " ")
  }, "")
}
# nome padronizado do grupo de fauna (aceita o que o campo costuma escrever)
grupo_padrao <- function(x) {
  g <- sem_acento(ifelse(is.na(x), "", x))
  out <- ifelse(grepl("anur|amphib|anfib|sapo|\\bras?\\b", g), "Anuros",
         ifelse(grepl("rept|squam|serp|lagart|testud|crocod|quelon|cobra", g), "Répteis",
         ifelse(grepl("^aves?$|ornit|passar|avifauna|\\bave", g), "Aves",
         ifelse(grepl("mamif|masto|mammal|quiropt|morceg|roedor|primat", g), "Mamíferos",
         ifelse(grepl("peixe|ictio|actinop", g), "Peixes",
         ifelse(grepl("inset|entomo|abelha|formiga|besouro|borbolet|mosquito|culicid|hymenop|coleop|lepidop|dipter", g), "Insetos",
         ifelse(grepl("aracn|aranha|escorpi", g), "Aracnídeos",
         ifelse(g == "", "Todas as espécies", tools::toTitleCase(norm_txt(x))))))))))
  out
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && !is.list(a) && is.na(a))) b else a

# Prefixo interno separa medidas ambientais dos campos conhecidos do cadastro.
catalogo_gradientes <- function(pontos) {
  colunas <- grep("^amb[.]", names(pontos), value = TRUE)
  nomes <- substring(colunas, 5L)
  ids <- vapply(nomes, function(nm) paste0("amb_", rlang::hash(enc2utf8(nm))), character(1), USE.NAMES = FALSE)
  data.frame(chave = ids, nome = nomes, stringsAsFactors = FALSE)
}

ler_planilha <- function(arquivo) {
  tmp <- copiar_planilha(arquivo)
  abas <- tryCatch(readxl::excel_sheets(tmp), error = function(e)
    stop("O arquivo não abre como planilha do Excel: pode estar corrompido ou ser de outro tipo com o nome terminado em .xlsx. ",
         "Abra no Excel e salve de novo como \"Pasta de Trabalho do Excel (.xlsx)\".", call. = FALSE))
  a_bru <- abas[grepl("brut|registro", sem_acento(abas))][1]
  if (is.na(a_bru)) {
    # sem aba de registros: monta os registros a partir da consolidada (espécie x P1..Pn)
    a_con0 <- abas[grepl("consolid", sem_acento(abas))][1]
    if (is.na(a_con0)) stop("Não achei a aba de registros (nome com 'brutos' ou 'registros') nem a consolidada. Abas: ",
                            paste(abas, collapse = ", "))
    d <- registros_da_consolidada(ler_consolidada(tmp, a_con0))
    if (is.null(d) || !nrow(d)) stop("A aba '", a_con0, "' não tem espécies com contagem nos pontos (colunas P1, P2...).")
    cat("Sem aba de registros: os dados foram montados a partir da aba '", a_con0, "'.\n", sep = "")
    a_bru <- paste0(a_con0, " (montada)")
  } else
  d <- as.data.frame(readxl::read_excel(tmp, sheet = a_bru, col_types = "text"))
  nm <- names(d)
  col <- list(
    ponto   = acha_coluna(nm, c("^ponto$", "ponto amostral", "^ponto", "sitio")),
    especie = acha_coluna(nm, c("nome valido", "^especie$", "especie")),
    familia = acha_coluna(nm, c("^familia")),
    grupo   = acha_coluna(nm, c("^grupo", "^classe")),
    vo      = acha_coluna(nm, c("^vo$", "vocaliza")),
    av      = acha_coluna(nm, c("^av$", "avista|visual")),
    semf    = acha_coluna(nm, c("sem forma")),
    total   = acha_coluna(nm, c("^total$", "abundancia", "^n$", "quantidade")),
    campanha= acha_coluna(nm, c("^campanha")),
    anot    = acha_coluna(nm, c("anotacao")),
    obs     = acha_coluna(nm, c("observ")),
    # colunas usadas nos relatórios de fauna (todas opcionais)
    metodo  = acha_coluna(nm, c("^metodo", "metodo de registro", "metodo de amostragem", "^tecnica")),
    estacao = acha_coluna(nm, c("sazonal", "^estacao")),
    dado    = acha_coluna(nm, c("tipo de dado", "^dado$", "origem do dado", "primario")),
    cinegetica = acha_coluna(nm, c("cinegetic")),
    xerimbabo  = acha_coluna(nm, c("xerimbabo", "^pet$")),
    medico     = acha_coluna(nm, c("interesse medico", "medico", "peconhent", "epidemiolog")),
    exotica    = acha_coluna(nm, c("exotic", "invasor")),
    bioindicadora = acha_coluna(nm, c("bioindicad", "indicadora")),
    migratoria = acha_coluna(nm, c("migrat")),
    guilda     = acha_coluna(nm, c("guilda", "dieta"))
  )
  if (is.na(col$ponto) || is.na(col$especie) || is.na(col$total))
    stop("A aba '", a_bru, "' precisa das colunas Ponto, Espécie e Total (ou Abundância).")
  pega <- function(k) if (is.na(col[[k]])) rep(NA_character_, nrow(d)) else norm_txt(d[[col[[k]]]])
  num  <- function(k) if (is.na(col[[k]])) rep(NA_real_, nrow(d)) else suppressWarnings(as.numeric(gsub(",", ".", d[[col[[k]]]])))
  reg <- data.frame(
    linha_planilha = seq_len(nrow(d)) + 1L,
    ponto_original = pega("ponto"),
    ponto    = norm_ponto(pega("ponto")),
    especie_completa = pega("especie"),
    familia  = pega("familia"),
    grupo    = pega("grupo"),
    vo = num("vo"), av = num("av"), sem_forma = num("semf"),
    total_txt = pega("total"),
    total = num("total"),
    campanha = pega("campanha"),
    anotacao = pega("anot"),
    observacao = pega("obs"),
    metodo = pega("metodo"), estacao = pega("estacao"), dado = pega("dado"),
    cinegetica = pega("cinegetica"), xerimbabo = pega("xerimbabo"), medico = pega("medico"), exotica = pega("exotica"),
    bioindicadora = pega("bioindicadora"), migratoria = pega("migratoria"), guilda = pega("guilda"),
    stringsAsFactors = FALSE
  )
  vazia <- (is.na(reg$especie_completa) | reg$especie_completa == "") &
           (is.na(reg$ponto_original) | reg$ponto_original == "")
  reg <- reg[!vazia, ]
  if (!nrow(reg)) stop("A aba '", a_bru, "' não tem nenhum registro (só o cabeçalho). ",
                       "Preencha uma linha por registro, com Ponto, Espécie e Total.", call. = FALSE)
  reg$especie <- binomio(reg$especie_completa)
  reg$genero  <- sub(" .*$", "", reg$especie)
  g <- sem_acento(ifelse(is.na(reg$grupo), "", reg$grupo))
  reg$grupo_pad <- grupo_padrao(reg$grupo)
  # sem coluna Campanha: se a estação (Sazonalidade, Estação) tem 2 ou mais valores, cada estação vira uma campanha (23/11/2026)
  campanha_da_estacao <- FALSE
  if (all(is.na(reg$campanha) | reg$campanha == "")) {
    ok_est <- !is.na(reg$estacao) & nzchar(reg$estacao)
    if (any(ok_est) && length(unique(reg$estacao[ok_est])) >= 2) { reg$campanha <- ifelse(ok_est, reg$estacao, NA_character_); campanha_da_estacao <- TRUE }
    else reg$campanha <- "Campanha única"
  }
  # dados secundários (bibliografia, entrevista) não entram nas estatísticas (exigência do IEMA-ES e boa prática)
  sec <- !is.na(reg$dado) & grepl("secund|entrevist|bibliogr|literat", sem_acento(reg$dado))
  secundarios <- reg[sec, , drop = FALSE]; reg <- reg[!sec, , drop = FALSE]

  # Aba de pontos (fitofisionomia, coordenadas, método)
  a_pt <- abas[grepl("ponto", sem_acento(abas)) & !grepl("brut", sem_acento(abas))][1]
  pontos <- NULL
  if (!is.na(a_pt)) {
    p <- as.data.frame(readxl::read_excel(tmp, sheet = a_pt, col_types = "text", .name_repair = "minimal"))
    # o modelo antigo tem uma linha de títulos acima do cabeçalho
    if (!any(grepl("sitio|ponto", sem_acento(names(p))))) {
      h <- which(apply(p, 1, function(r) any(grepl("sitio|ponto", sem_acento(r)))))[1]
      if (!is.na(h)) { names(p) <- norm_txt(unlist(p[h, ])); p <- p[-(1:h), , drop = FALSE] }
    }
    cp <- acha_coluna(names(p), c("^ponto$", "^sitio$", "sitio", "ponto"))
    cf <- acha_coluna(names(p), c("fitofision", "ambiente", "habitat"))
    cc <- acha_coluna(names(p), c("coorden"))
    cm <- acha_coluna(names(p), c("metodo"))
    cg <- acha_coluna(names(p), c("grupo amostral", "^grupo", "tratamento"))
    ce <- acha_coluna(names(p), c("esforco", "horas"))
    ccam <- acha_coluna(names(p), c("^campanha$"))
    if (!is.na(cp)) {
      pontos <- data.frame(
        ponto = norm_ponto(p[[cp]]),
        nome = norm_txt(p[[cp]]),
        fitofisionomia = if (!is.na(cf)) norm_txt(p[[cf]]) else NA,
        coordenadas = if (!is.na(cc)) norm_txt(p[[cc]]) else NA,
        metodo = if (!is.na(cm)) norm_txt(p[[cm]]) else NA,
        grupo_amostral = if (!is.na(cg)) norm_txt(p[[cg]]) else NA,
        esforco = if (!is.na(ce)) suppressWarnings(as.numeric(gsub(",", ".", p[[ce]]))) else NA,
        stringsAsFactors = FALSE)
      extras <- setdiff(seq_along(p), c(cp, cf, cc, cm, cg, ce, ccam))
      rotulos <- norm_txt(names(p))
      for (j in extras) {
        nm <- rotulos[j]
        if (is.na(nm) || !nzchar(nm) || sum(rotulos == nm, na.rm = TRUE) != 1L) next
        v <- trimws(p[[j]])
        v[toupper(v) %in% c("", "-", "NA", "N/A", "ND")] <- NA_character_
        x <- suppressWarnings(as.numeric(chartr(",", ".", v)))
        if (any(is.finite(x)) && all(is.na(v) | is.finite(x))) pontos[[paste0("amb.", nm)]] <- x
      }
      if (!is.na(ccam)) pontos$campanha <- norm_txt(p[[ccam]])
      pontos <- pontos[!is.na(pontos$nome) & nzchar(pontos$nome), , drop = FALSE]
    }
  }

  # Aba consolidada (para o corretor comparar com os registros)
  a_con <- abas[grepl("consolid", sem_acento(abas))][1]
  consol <- if (!is.na(a_con)) ler_consolidada(tmp, a_con) else NULL
  # família e grupo de cada espécie da consolidada (vêm dos registros)
  if (!is.null(consol)) {
    consol$familia <- reg$familia[match(consol$especie, reg$especie)]
    consol$grupo <- reg$grupo_pad[match(consol$especie, reg$especie)]
    if (!is.null(consol$familia_cons)) { consol$familia <- ifelse(is.na(consol$familia), consol$familia_cons, consol$familia); consol$grupo <- ifelse(is.na(consol$grupo), consol$grupo_cons, consol$grupo) }
  }
  list(arquivo = arquivo, aba_registros = a_bru, aba_pontos = a_pt, aba_consolidada = a_con,
       registros = reg, pontos = pontos, consolidada = consol, secundarios = secundarios, campanha_da_estacao = campanha_da_estacao)
}

total_de_campo_valido <- function(x) is.finite(x) & x >= 0 & x == floor(x)
somar_totais_de_campo <- function(x) sum(x[total_de_campo_valido(x)])

# Tira das análises os registros que o corretor marca como erro e que quebram ou distorcem os cálculos:
# espécie vazia (viraria uma "espécie" sem nome e aumentaria a riqueza) e Total vazio, não numérico ou negativo
# (o vegan não aceita negativo). Eles continuam na conferência e ficam em L$fora_da_analise com o motivo.
registros_para_analise <- function(L) {
  r <- L$registros
  sem_esp <- is.na(r$especie_completa) | trimws(r$especie_completa) == "" | is.na(r$especie) | trimws(r$especie) == ""
  tot_ruim <- !total_de_campo_valido(r$total)
  sem_ponto <- is.na(r$ponto) | !nzchar(trimws(r$ponto))
  sem_campanha <- is.na(r$campanha) | !nzchar(trimws(r$campanha))
  fora <- sem_esp | tot_ruim | sem_ponto | sem_campanha
  L$fora_da_analise <- r[fora, , drop = FALSE]
  motivo <- ifelse(sem_esp, "sem nome de espécie", ifelse(sem_ponto, "sem ponto",
    ifelse(sem_campanha, "campanha não informada", "Total deve ser inteiro, finito e não negativo")))
  L$fora_da_analise$motivo <- motivo[fora]
  L$registros <- r[!fora, , drop = FALSE]
  L
}

# transforma a consolidada (espécie x P1..Pn) em registros: uma linha por espécie e ponto com contagem > 0,
# com as colunas da aba de registros (Ponto, Espécie, Família, Grupo, Total)
registros_da_consolidada <- function(consol) {
  if (is.null(consol) || !nrow(consol)) return(NULL)
  cps <- grep("^P\\d+$", names(consol), value = TRUE)
  out <- list()
  for (i in seq_len(nrow(consol))) for (p in cps) {
    n <- consol[[p]][i]
    if (is.na(n) || n <= 0) next
    # VO e AV ficam em branco: a consolidada não diz quantos indivíduos de cada forma (a forma vem da coluna dela)
    out[[length(out) + 1]] <- data.frame(
      Ponto = p, `Espécie` = consol$especie_completa[i],
      `Família` = consol$familia_cons[i] %||% NA_character_, Grupo = consol$grupo_cons[i] %||% NA_character_,
      Total = as.character(n), `Observação` = "montado da aba consolidada",
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# lê a aba consolidada (espécie x P1..Pn, cabeçalho em duas linhas) com os atributos das espécies
ler_consolidada <- function(tmp, a_con) {
  consol <- NULL
    k <- as.data.frame(readxl::read_excel(tmp, sheet = a_con, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    h <- which(apply(k, 1, function(r) any(grepl("^P ?\\d+$", norm_txt(r)))))[1]
    if (!is.na(h) && h < nrow(k)) {
      cab <- norm_txt(unlist(k[h, ]))
      cps <- which(grepl("^P ?\\d+$", cab))
      ctot <- which(sem_acento(cab) == "total")[1]
      linhas <- k[(h + 1):nrow(k), , drop = FALSE]
      nomes <- norm_txt(linhas[[1]])
      vals <- do.call(cbind, lapply(cps, function(j) suppressWarnings(as.numeric(gsub(",", ".", ifelse(norm_txt(linhas[[j]]) %in% c("-", "–", ""), "0", linhas[[j]]))))))
      tem <- rowSums(!is.na(vals)) > 0 & !is.na(nomes) & nzchar(sem_acento(nomes)) & !grepl("^(total|soma|riqueza|abundancia|numero de|no? de |n[^a-z ]{1,2} ?de )", sem_acento(nomes))
      if (any(tem)) {
        consol <- data.frame(linha_planilha = which(tem) + h, especie_completa = nomes[tem],
                             especie = binomio(nomes[tem]), stringsAsFactors = FALSE)
        m <- vals[tem, , drop = FALSE]; colnames(m) <- gsub(" ", "", cab[cps])
        consol <- cbind(consol, as.data.frame(m))
        consol$total_informado <- if (!is.na(ctot)) suppressWarnings(as.numeric(gsub(",", ".", linhas[[ctot]][tem]))) else NA
        # atributos das espécies (cabeçalho em duas linhas: usa a de baixo quando preenchida)
        cab1 <- if (h > 1) norm_txt(unlist(k[h - 1, ])) else rep("", length(cab))
        cab2 <- sem_acento(ifelse(is.na(cab) | cab == "", cab1, cab)); cab1s <- sem_acento(cab1)
        pega_at <- function(pad, onde = cab2) { j <- which(grepl(pad, onde))[1]; if (is.na(j)) rep(NA_character_, sum(tem)) else { v <- norm_txt(linhas[[j]][tem]); v[v %in% c("", "-", "–")] <- NA; v } }
        consol$nome_comum <- pega_at("nome comum", cab1s)
        consol$endemismo <- pega_at("^end")
        consol$habito <- pega_at("^hab")
        consol$dependencia <- pega_at("^dep")
        consol$sensibilidade <- pega_at("^sens")
        consol$forma_registro <- pega_at("forma")
        consol$salve <- pega_at("salve|icmbio|mma")
        consol$iucn <- pega_at("iucn")
        # família e grupo pelas linhas de título acima de cada espécie (ex.: "Anuros", "Bufonidae")
        fam <- grp <- NA_character_; fam_v <- grp_v <- rep(NA_character_, length(nomes))
        g0 <- norm_txt(k[h, 1]); if (!is.na(g0) && !grepl(" ", g0)) { g0 <- grupo_padrao(g0); if (g0 != "Todas as espécies") grp <- g0 }
        for (j in seq_along(nomes)) {
          t <- nomes[j]; if (is.na(t) || t == "") next
          if (tem[j]) { fam_v[j] <- fam; grp_v[j] <- grp; next }
          if (!grepl(" ", t) || grepl("^[A-Z][a-z]+idae\\b|^[A-Z][a-z]+inae\\b", t)) {
            if (grepl("idae\\b|inae\\b", t)) fam <- t else { g <- grupo_padrao(t); if (g != "Todas as espécies") grp <- g }
          }
        }
        consol$familia_cons <- fam_v[tem]; consol$grupo_cons <- grp_v[tem]
      }
    }
  consol
}
