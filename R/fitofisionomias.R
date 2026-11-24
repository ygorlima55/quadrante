# Quadrante: análise por fitofisionomia dos pontos (coluna "Fitofisionomia" da aba de pontos).
# Duas leituras:
# - classe como está escrita (ex.: "MG/VR" é uma classe);
# - componente: cada sigla de um ponto misto conta separado (P1 "MG/VR" entra em MG e em VR).
# Para cada uma: pontos, espécies, indivíduos, espécies exclusivas e a lista; similaridade (Jaccard) entre fitofisionomias.

partes_fito <- function(x) { v <- trimws(unlist(strsplit(x, "[/+,;]| e ", perl = TRUE))); v[nzchar(v)] }

# nomes das siglas: arquivo siglas_fitofisionomias.csv na pasta do programa (sigla;nome), editável.
# A aba de pontos também pode trazer uma coluna "Legenda" ou outra aba "Siglas" no futuro; por ora, o arquivo.
SIGLAS_FITO <- local({
  f <- file.path(dirname(dirname(normalizePath(sys.frame(1)$ofile %||% "R/x.R", mustWork = FALSE))), "siglas_fitofisionomias.csv")
  if (!file.exists(f)) f <- "siglas_fitofisionomias.csv"
  if (file.exists(f)) { d <- utils::read.csv2(f, encoding = "UTF-8", fileEncoding = "UTF-8", stringsAsFactors = FALSE); setNames(trimws(d$nome), toupper(trimws(d$sigla))) } else character()
})
nome_fito <- function(x) vapply(x, function(f) { p <- partes_fito(f); if (!length(p)) return(f)
  n <- ifelse(toupper(p) %in% names(SIGLAS_FITO), SIGLAS_FITO[toupper(p)], p); paste(n, collapse = " + ") }, "", USE.NAMES = FALSE)

analisar_fito <- function(m, pontos_info, fam = NULL) {
  if (is.null(pontos_info) || all(is.na(pontos_info$fitofisionomia))) return(NULL)
  fito <- pontos_info$fitofisionomia[match(rownames(m), pontos_info$ponto)]
  if (all(is.na(fito))) return(NULL)
  fito[is.na(fito) | fito == ""] <- "Sem fitofisionomia"
  ok <- rowSums(m) >= 0
  resumo <- function(grupos) {   # grupos: lista nome -> pontos
    nomes <- names(grupos)
    pres <- t(vapply(grupos, function(p) colSums(m[p, , drop = FALSE]) > 0, logical(ncol(m))))
    ab <- t(vapply(grupos, function(p) colSums(m[p, , drop = FALSE]), numeric(ncol(m))))
    if (length(grupos) == 1) { pres <- matrix(pres, 1, dimnames = list(nomes, colnames(m))); ab <- matrix(ab, 1, dimnames = list(nomes, colnames(m))) }
    excl <- vapply(seq_along(nomes), function(i) sum(pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0), 1L)
    tab <- data.frame(fitofisionomia = nomes, nome = nome_fito(nomes), pontos = vapply(grupos, paste, "", collapse = ", "), n_pontos = lengths(grupos),
      riqueza = rowSums(pres), abundancia = rowSums(ab), riqueza_media_por_ponto = round(vapply(grupos, function(p) mean(rowSums(m[p, , drop = FALSE] > 0)), 1), 1),
      exclusivas = excl,
      shannon = round(apply(ab, 1, function(v) if (sum(v)) vegan::diversity(v, "shannon") else NA_real_), 3),
      hill1 = round(apply(ab, 1, function(v) if (sum(v)) exp(vegan::diversity(v, "shannon")) else NA_real_), 2),
      hill2 = round(apply(ab, 1, function(v) if (sum(v) > 0) 1 / sum((v / sum(v))^2) else NA), 2),
      pielou = round(apply(ab, 1, function(v) { s <- sum(v > 0); if (s > 1) vegan::diversity(v, "shannon") / log(s) else NA }), 3),
      especies_exclusivas = vapply(seq_along(nomes), function(i) paste(colnames(m)[pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0], collapse = "; "), ""),
      stringsAsFactors = FALSE)
    tab <- tab[order(-tab$riqueza), ]; rownames(tab) <- NULL
    esp <- data.frame(familia = if (!is.null(fam)) unname(fam[colnames(m)]) else NA, especie = colnames(m), t(ab), check.names = FALSE, stringsAsFactors = FALSE)
    esp$em <- apply(pres, 2, function(v) paste(rownames(pres)[v], collapse = ", "))
    esp <- esp[order(esp$familia, esp$especie), ]; rownames(esp) <- NULL
    pares <- if (length(nomes) >= 2) do.call(rbind, combn(nomes, 2, simplify = FALSE, FUN = function(p) {
      a <- pres[p[1], ]; b <- pres[p[2], ]; data.frame(fito_a = p[1], fito_b = p[2], comuns = sum(a & b), so_a = sum(a & !b), so_b = sum(b & !a),
        jaccard = if (any(a | b)) round(sum(a & b) / sum(a | b), 3) else NA_real_, stringsAsFactors = FALSE) })) else NULL
    if (!is.null(pares)) pares <- pares[order(-pares$jaccard), ]
    fams <- if (!is.null(fam)) do.call(rbind, lapply(nomes, function(nm) { e <- colnames(m)[pres[nm, ]]; if (!length(e)) return(NULL)
      t <- table(fam[e]); data.frame(fitofisionomia = nm, familia = names(t), especies = as.integer(t), stringsAsFactors = FALSE) })) else NULL
    jac <- matrix(1, length(nomes), length(nomes), dimnames = list(nomes, nomes))
    for (i in seq_along(nomes)) for (j in seq_along(nomes)) {
      uniao <- sum(pres[i, ] | pres[j, ])
      jac[i, j] <- if (uniao) round(sum(pres[i, ] & pres[j, ]) / uniao, 3) else NA_real_
    }
    rar <- NULL
    vs <- lapply(nomes, function(nm) { v <- sort(ab[nm, ab[nm, ] > 0], decreasing = TRUE); v })
    names(vs) <- nomes; vs <- vs[vapply(vs, function(v) length(v) >= 2 && sum(v) >= 4, TRUE)]
    if (length(vs)) {
      o <- tryCatch(suppressWarnings(iNEXT::iNEXT(vs, q = 0, datatype = "abundance", knots = 40, nboot = 50)), error = function(e) NULL)
      if (!is.null(o)) { z <- o$iNextEst$size_based
        rar <- data.frame(fitofisionomia = as.character(z$Assemblage), m = z$m, metodo = as.character(z$Method), qD = round(z$qD, 3), li = round(z$qD.LCL, 3), ls = round(z$qD.UCL, 3))
        a <- o$AsyEst; a <- a[a$Diversity == "Species richness", ]
        tab$chao1 <- round(a$Estimator[match(tab$fitofisionomia, a$Assemblage)], 1) }
    }
    list(tabela = tab, especies = esp, pares = pares, familias = fams, jaccard = jac, rarefacao = rar)
  }
  cls <- split(rownames(m), fito)
  comp_nomes <- unique(unlist(lapply(fito, partes_fito)))
  comp <- setNames(lapply(comp_nomes, function(cn) rownames(m)[vapply(fito, function(f) cn %in% partes_fito(f), TRUE)]), comp_nomes)
  A <- resumo(cls); B <- if (length(comp) >= 2 && !identical(sort(names(comp)), sort(names(cls)))) resumo(comp) else NULL
  t1 <- A$tabela; um <- t1$n_pontos == 1
  txtres <- sprintf("%d fitofisionomias como escritas na planilha (%s). A mais rica foi %s (%d espécies em %d ponto%s) e a com mais espécies só dela foi %s (%d exclusivas).%s",
    nrow(t1), paste(sprintf("%s: %s", t1$nome, t1$pontos), collapse = "; "), t1$nome[1], t1$riqueza[1], t1$n_pontos[1], if (t1$n_pontos[1] > 1) "s" else "",
    t1$nome[which.max(t1$exclusivas)], max(t1$exclusivas),
    if (!is.null(B)) sprintf(" Contando cada sigla separada: %s.", paste(sprintf("%s %d espécies (%d %s)", B$tabela$nome, B$tabela$riqueza, B$tabela$n_pontos, ifelse(B$tabela$n_pontos == 1, "ponto", "pontos")), collapse = "; ")) else "")
  GA <- NULL
  ga <- pontos_info$grupo_amostral[match(rownames(m), pontos_info$ponto)]
  if (any(!is.na(ga) & nzchar(ga)) && length(unique(na.omit(ga))) >= 2) {
    vs <- lapply(split(rownames(m)[!is.na(ga)], ga[!is.na(ga)]), function(p) { v <- sort(colSums(m[p, , drop = FALSE]), decreasing = TRUE); v[v > 0] })
    vs <- vs[vapply(vs, function(v) length(v) >= 2 && sum(v) >= 4, TRUE)]
    if (length(vs) >= 2) {
      o <- tryCatch(suppressWarnings(iNEXT::iNEXT(vs, q = 0, datatype = "abundance", knots = 40, nboot = 50)), error = function(e) NULL)
      if (!is.null(o)) { z <- o$iNextEst$size_based
        GA <- list(rarefacao = data.frame(fitofisionomia = as.character(z$Assemblage), m = z$m, metodo = as.character(z$Method), qD = round(z$qD, 3), li = round(z$qD.LCL, 3), ls = round(z$qD.UCL, 3)),
                   titulo = if (!is.null(pontos_info$campanha) && all(names(vs) %in% pontos_info$campanha)) "Campanha" else "Ambiente") }
    }
  }
  list(classes = A, componentes = B, grupos_amostrais = GA, texto = txt(
    "Junta os pontos pela fitofisionomia da aba de pontos e compara o que cada ambiente tem: espécies, indivíduos, espécies que só aparecem nele (exclusivas) e quanto as listas se parecem (Jaccard). Em pontos mistos (ex.: MG/VR), a segunda leitura conta cada sigla separada, porque o ponto tem os dois ambientes.",
    txtres,
    paste0("Fitofisionomias com espécies exclusivas são as que mais contribuem para a riqueza da área: perder esse ambiente leva espécies que os outros não têm. Ambientes úmidos (veredas, matas de galeria, brejos) costumam concentrar anuros; formações abertas e rupestres podem ter espécies especialistas. Jaccard baixo entre fitofisionomias indica comunidades diferentes (alta diversidade beta entre ambientes).",
      if (any(grepl("antropiz", nome_fito(t1$fitofisionomia), ignore.case = TRUE))) " Em áreas antropizadas, espécies generalistas e tolerantes costumam dominar; espécies exclusivas desses pontos tendem a ser de áreas abertas, e não especialistas de mata." else ""),
    if (any(um)) sprintf("%d das %d fitofisionomias têm um ponto só (%s): a comparação é descritiva, porque não dá para separar o ambiente do ponto. Riqueza depende do número de pontos e do esforço; compare pela riqueza média por ponto. Testes (PERMANOVA) só com 2 ou mais pontos em cada fitofisionomia. Nomes das siglas: arquivo siglas_fitofisionomias.csv.",
      sum(um), nrow(t1), paste(t1$nome[um], collapse = ", ")) else "Riqueza depende do número de pontos e do esforço; compare pela riqueza média por ponto. Nomes das siglas: arquivo siglas_fitofisionomias.csv."))
}

catalogo_fito <- function() list(
  fito_riqueza = list(nome = "Fitofisionomias: espécies e exclusivas", padrao = "classes", formas = list(
    classes = "Fitofisionomia como escrita (MG/VR...)", componentes = "Cada sigla separada (pontos mistos contam nas duas)")),
  fito_abundancia = list(nome = "Fitofisionomias: indivíduos", padrao = "classes", formas = list(
    classes = "Fitofisionomia como escrita (MG/VR...)", componentes = "Cada sigla separada (pontos mistos contam nas duas)")),
  fito_diversidade = list(nome = "Fitofisionomias: diversidade (Hill)", padrao = "hill", formas = list(hill = "Riqueza, espécies comuns e dominantes, com valores")),
  fito_rarefacao = list(nome = "Fitofisionomias: rarefação (iNEXT)", padrao = "classico", formas = list(
    classico = "Por fitofisionomia: interpolação cheia, extrapolação tracejada, faixa preenchida", inext = "Curvas de cada fitofisionomia, estilo iNEXT")),
  rarefacao_ambientes = list(nome = "Rarefação por ambiente (grupo amostral)", padrao = "classico", formas = list(
    classico = "Por grupo amostral (ex.: mata x aberto): interpolação cheia, extrapolação tracejada, faixa preenchida")),
  fito_familias = list(nome = "Fitofisionomias: famílias", padrao = "colunas", formas = list(colunas = "Famílias por fitofisionomia")),
  fito_similaridade = list(nome = "Fitofisionomias: similaridade (Jaccard)", padrao = "ladrilhos", formas = list(ladrilhos = "Mapa com os valores de Jaccard")),
  fitofisionomias_especies = list(nome = "Fitofisionomias: espécies em cada uma", padrao = "matriz", formas = list(matriz = "Espécie x fitofisionomia, círculo do tamanho da abundância"))
)

desenhar_fito <- function(chave, forma, G, e, gg, pal, k, w, h) {
  F <- G$fitofisionomias; if (is.null(F)) return(invisible()); tam <- e$fonte / 3.1
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  quebra <- function(x) vapply(x, function(s) paste(strwrap(s, width = 15), collapse = "\n"), "", USE.NAMES = FALSE)   # nomes longos em linhas curtas, sem sobrepor
  rotulo <- function(t) factor(paste0(quebra(t$nome %||% t$fitofisionomia), "\n(", t$pontos, ")"), levels = unique(paste0(quebra(t$nome %||% t$fitofisionomia), "\n(", t$pontos, ")")))
  cores_f <- function(nomes) setNames(rep(pal, length.out = length(nomes)), nomes)
  if (chave %in% c("fito_riqueza", "fito_abundancia")) {
    X <- if (forma == "componentes" && !is.null(F$componentes)) F$componentes else F$classes; t <- X$tabela; t <- t[t$riqueza > 0, , drop = FALSE]; t$rot <- rotulo(t)   # sem registro não entra
    if (chave == "fito_riqueza") {
      d <- rbind(data.frame(rot = t$rot, medida = "Espécies", v = t$riqueza), data.frame(rot = t$rot, medida = "Exclusivas", v = t$exclusivas))
      d$medida <- factor(d$medida, levels = unique(d$medida))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, v, fill = medida)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75) +
        ggplot2::geom_text(ggplot2::aes(label = v), position = ggplot2::position_dodge(.8), vjust = -.4, family = e$familia, size = tam, colour = k$tinta) +
        ggplot2::scale_fill_manual(values = c(pal[1], pal[4]), name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
        titulos_gg(e, chave, if (forma == "componentes") "Espécies por fitofisionomia (cada sigla separada)" else "Espécies por fitofisionomia", NULL, "Número de espécies",
                   "Exclusivas = espécies registradas só naquela fitofisionomia.") + tema_gg(e), w, h))
    }
    return(gg(ggplot2::ggplot(t, ggplot2::aes(rot, abundancia)) + ggplot2::geom_col(fill = pal[2], width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = abundancia), vjust = -.4, family = e$familia, size = tam * 1.05, colour = k$tinta) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
      titulos_gg(e, chave, if (forma == "componentes") "Indivíduos por fitofisionomia (cada sigla separada)" else "Indivíduos por fitofisionomia", NULL, "Número de indivíduos") + tema_gg(e), w, h))
  }
  if (chave == "fito_diversidade") {
    t <- F$classes$tabela; t <- t[t$riqueza > 0, , drop = FALSE]; t$rot <- rotulo(t)
    nh <- c("Riqueza", "Shannon", "Simpson")
    d <- rbind(data.frame(rot = t$rot, h = nh[1], v = t$riqueza), data.frame(rot = t$rot, h = nh[2], v = t$hill1), data.frame(rot = t$rot, h = nh[3], v = t$hill2)); d$h <- factor(d$h, levels = nh)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, v, fill = h)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75, na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(abs(v - round(v)) < 1e-9, formatC(round(v), format = "d"), fmt1(v))), position = ggplot2::position_dodge(.8), vjust = -.4, family = e$familia, size = tam * .9, colour = k$tinta, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = pal[1:3], name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
      titulos_gg(e, chave, "Diversidade por fitofisionomia", NULL, "Número efetivo de espécies",
                 "Colunas de Shannon e Simpson baixas em relação à riqueza = poucas espécies dominam.") + tema_gg(e), w, h * 1.1))
  }
  if ((chave == "fito_rarefacao" && forma == "classico" && !is.null(F$classes$rarefacao)) || (chave == "rarefacao_ambientes" && !is.null(F$grupos_amostrais$rarefacao))) {
    if (chave == "fito_rarefacao") { z <- F$classes$rarefacao; t <- F$classes$tabela; nm <- setNames(t$nome %||% t$fitofisionomia, t$fitofisionomia); tit <- "Fitofisionomia" }
    else { z <- F$grupos_amostrais$rarefacao; nm <- setNames(unique(z$fitofisionomia), unique(z$fitofisionomia)); tit <- F$grupos_amostrais$titulo }
    return(gg(rarefacao_classica(chave, z, nm, tit, e, pal, k), w, h * 1.15))
  }
  if (chave == "fito_rarefacao" && !is.null(F$classes$rarefacao)) {
    z <- F$classes$rarefacao; t <- F$classes$tabela; nm <- setNames(t$nome %||% t$fitofisionomia, t$fitofisionomia)
    z$fito <- factor(nm[z$fitofisionomia], levels = unique(nm[t$fitofisionomia]))
    z$parte <- ifelse(z$metodo == "Extrapolation", "Extrapolação (projeção)", "Rarefação (dentro da amostra)")
    lig <- z[z$metodo == "Observed", ]; ob <- lig; lig$parte <- "Extrapolação (projeção)"; z <- rbind(z, lig)
    cs <- cores_f(levels(z$fito))
    return(gg(ggplot2::ggplot(z, ggplot2::aes(m, qD, colour = fito, fill = fito)) +
      faixa_ic(e, z[!duplicated(paste(z$fito, z$m)), ], cs, "fito", "m") +
      ggplot2::geom_line(ggplot2::aes(linetype = parte, group = interaction(fito, parte)), linewidth = e$linha * 1.2) +
      ggplot2::geom_point(data = ob, size = e$ponto * 1.8) +
      ggplot2::geom_text(data = ob, ggplot2::aes(label = fmt1(qD, 0)), vjust = -1, family = e$familia, size = tam, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = cs, name = "Fitofisionomia") + ggplot2::scale_fill_manual(values = cs, guide = "none") +
      ggplot2::scale_linetype_manual(values = c("Rarefação (dentro da amostra)" = "solid", "Extrapolação (projeção)" = "22"), name = "Curva") +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .08))) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, order = 1), linetype = ggplot2::guide_legend(ncol = 1, order = 2)) +
      titulos_gg(e, chave, "Riqueza por número de indivíduos em cada fitofisionomia", "Número de indivíduos", "Número de espécies",
                 "Ponto = amostra real. Compare as curvas no mesmo número de indivíduos (rarefação). iNEXT, faixas = IC 95%.") +
      tema_gg(e) + ggplot2::theme(legend.box = "vertical", legend.box.just = "left", legend.title.position = "top"), w, h * 1.35))
  }
  if (chave == "fito_familias" && !is.null(F$classes$familias)) {
    d <- F$classes$familias; t <- F$classes$tabela; nm <- setNames(quebra(t$nome %||% t$fitofisionomia), t$fitofisionomia)
    d$fito <- factor(nm[d$fitofisionomia], levels = unique(nm[t$fitofisionomia]))
    cf <- cores_familias(d$familia, pal)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(fito, especies, fill = familia)) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single", padding = .12), width = .85) +
      ggplot2::geom_text(ggplot2::aes(label = especies, group = familia), position = ggplot2::position_dodge2(width = .85, preserve = "single", padding = .12), vjust = -.4, family = e$familia, size = tam * .9, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12)), breaks = function(l) unique(floor(pretty(l)))) +
      titulos_gg(e, chave, "Famílias por fitofisionomia", NULL, "Número de espécies") + tema_gg(e), w * 1.15, h))
  }
  if (chave == "fito_similaridade" && !is.null(F$classes$jaccard) && nrow(F$classes$jaccard) >= 2) {
    J <- F$classes$jaccard; t <- F$classes$tabela; embrulha <- function(s) vapply(strsplit(s, "
", fixed = TRUE), function(ls) paste(unlist(lapply(ls, strwrap, width = 18)), collapse = "
"), "")
    nm <- setNames(paste0(embrulha(quebra(t$nome %||% t$fitofisionomia)), "
(", t$pontos, ")"), t$fitofisionomia)
    d <- expand.grid(a = rownames(J), b = colnames(J), stringsAsFactors = FALSE); d$v <- J[cbind(d$a, d$b)]
    lv <- unname(nm[rownames(J)]); d$a <- factor(nm[d$a], levels = lv); d$b <- factor(nm[d$b], levels = rev(lv))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(a, b, fill = v)) + ggplot2::geom_tile(colour = k$fundo, linewidth = 1.2) +
      ggplot2::geom_text(ggplot2::aes(label = fmt1(v, 2)), family = e$familia, size = tam * 1.1, colour = ifelse(d$v > .5, "white", k$tinta)) +
      ggplot2::scale_fill_gradient(low = "#f1eee6", high = pal[1], limits = c(0, 1), name = "Jaccard", breaks = c(0, .5, 1), labels = function(x) fmt1(x, 1),
                                   guide = ggplot2::guide_colourbar(barheight = grid::unit(8, "lines"), barwidth = grid::unit(.8, "lines"))) + ggplot2::coord_equal() +
      titulos_gg(e, chave, "Similaridade entre fitofisionomias", NULL, NULL, "1 = mesmas espécies; 0 = nenhuma em comum.") + tema_gg(e) +
      ggplot2::theme(legend.position = "right", legend.direction = "vertical", legend.justification = "center",
                     axis.text.x = ggplot2::element_text(size = ggplot2::rel(.85), lineheight = .9), axis.text.y = ggplot2::element_text(size = ggplot2::rel(.85), lineheight = .9)),
      h * 1.75, h * 1.25))
  }
  rotx <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1))
  if (chave == "fitofisionomias") {
    X <- if (forma == "componentes" && !is.null(F$componentes)) F$componentes else F$classes; t <- X$tabela
    t$rot <- paste0(gsub(" \\+ ", " +\n", t$nome %||% t$fitofisionomia), "\n(", t$pontos, ")")
    t$rot <- factor(t$rot, levels = t$rot)
    d <- rbind(data.frame(rot = t$rot, medida = "Espécies", v = t$riqueza), data.frame(rot = t$rot, medida = "Espécies exclusivas", v = t$exclusivas),
               data.frame(rot = t$rot, medida = "Indivíduos", v = t$abundancia))
    d$painel <- ifelse(d$medida == "Indivíduos", "Indivíduos", "Espécies (total e só daquela fitofisionomia)")
    d$painel <- factor(d$painel, levels = c("Espécies (total e só daquela fitofisionomia)", "Indivíduos")); d$rot <- factor(d$rot, levels = levels(t$rot)); d$medida <- factor(d$medida, levels = c("Espécies", "Espécies exclusivas", "Indivíduos"))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, v, fill = medida)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75) +
      ggplot2::geom_text(ggplot2::aes(label = v), position = ggplot2::position_dodge(.8), vjust = -.4, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::facet_wrap(~painel, ncol = 1, scales = "free_y") + ggplot2::scale_fill_manual(values = c(pal[1], pal[4], pal[2]), name = NULL) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .18))) +
      titulos_gg(e, chave, if (forma == "componentes") "Espécies e indivíduos por fitofisionomia (cada sigla separada)" else "Espécies e indivíduos por fitofisionomia",
                 NULL, NULL, "Exclusivas = espécies registradas só naquela fitofisionomia.") + tema_gg(e), w * 1.05, h * 1.45))
  }
  if (chave == "fitofisionomias_especies") {
    E <- F$classes$especies; fs <- F$classes$tabela$fitofisionomia; nm <- setNames(gsub(" \\+ ", " +\n", F$classes$tabela$nome %||% fs), fs)
    d <- do.call(rbind, lapply(fs, function(f) data.frame(especie = E$especie, fito = nm[[f]], n = E[[f]])))
    d <- d[d$n > 0, ]; d$especie <- factor(d$especie, levels = rev(E$especie)); d$fito <- factor(d$fito, levels = unname(nm))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(fito, especie)) + ggplot2::geom_point(ggplot2::aes(size = n), colour = pal[1], alpha = .85, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = n), nudge_x = .3, hjust = 0, family = e$familia, size = tam * .85, colour = k$tinta) +
      ggplot2::scale_size_area(max_size = 8) + titulos_gg(e, chave, "Lista de espécies por fitofisionomia", NULL, NULL, "Tamanho do círculo e número = indivíduos.") +
      tema_gg(e) + ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")), w * .9, max(h, .2 * nrow(E) + 1.4)))
  }
  invisible()
}


# ---- rarefação e extrapolação por grupo no estilo das curvas de rarefação de artigos (17/11/2026) ----
# interpolação = linha cheia, extrapolação = tracejada, faixa de IC preenchida na cor do grupo,
# marcador grande no tamanho real da amostra, com forma diferente por grupo; legenda "Método" e o nome do agrupamento.
rarefacao_classica <- function(chave, z, nomes, titulo, e, pal, k) {
  z$grupo <- factor(nomes[z$fitofisionomia], levels = unique(unname(nomes)))
  z$parte <- factor(ifelse(z$metodo == "Extrapolation", "Extrapolação", "Interpolação"), levels = c("Interpolação", "Extrapolação"))
  ob <- z[z$metodo == "Observed", ]; lig <- ob; lig$parte <- factor("Extrapolação", levels = levels(z$parte)); z2 <- rbind(z, lig)
  ob$parte <- factor("Interpolação", levels = levels(z$parte)); z2 <- rbind(z2, ob)
  z2 <- z2[order(z2$grupo, z2$m), ]
  gs <- levels(z$grupo); cs <- setNames(rep(pal, length.out = length(gs)), gs); fs <- setNames(rep(c(16, 17, 15, 18, 25, 8), length.out = length(gs)), gs)
  rib <- z[!duplicated(paste(z$grupo, z$m)), ]; rib <- rib[order(rib$grupo, rib$m), ]
  ggplot2::ggplot(z2, ggplot2::aes(m, qD, colour = grupo, fill = grupo)) +
    { if (isTRUE(e$faixa)) ggplot2::geom_ribbon(data = rib, ggplot2::aes(ymin = li, ymax = ls, group = grupo), alpha = .35, colour = NA) } +
    ggplot2::geom_line(ggplot2::aes(linetype = parte, group = interaction(grupo, parte)), linewidth = e$linha * 1.25) +
    ggplot2::geom_point(data = ob, ggplot2::aes(shape = grupo), size = e$ponto * 2.1) +
    ggplot2::scale_colour_manual(values = cs, name = titulo) + ggplot2::scale_fill_manual(values = cs, name = titulo) +
    ggplot2::scale_shape_manual(values = fs, name = titulo) +
    ggplot2::scale_linetype_manual(values = c("Interpolação" = "solid", "Extrapolação" = "22"), name = "Método") +
    ggplot2::guides(linetype = ggplot2::guide_legend(order = 1, override.aes = list(colour = k$tinta, fill = NA, linewidth = .8)),
                    colour = ggplot2::guide_legend(order = 2), fill = ggplot2::guide_legend(order = 2), shape = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(.02, .05))) +
    ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL, x = rot(e, chave, "x", "Número de indivíduos"), y = rot(e, chave, "y", "Número de espécies")) +
    ggplot2::theme_classic(e$fonte, e$familia) +
    ggplot2::theme(text = ggplot2::element_text(colour = k$tinta), axis.text = ggplot2::element_text(colour = k$tinta2),
      panel.border = ggplot2::element_rect(fill = NA, colour = k$tinta, linewidth = .5), axis.line = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA), panel.background = ggplot2::element_rect(fill = k$fundo, colour = NA),
      legend.position = c(.98, .04), legend.justification = c(1, 0), legend.box.just = "left",
      legend.background = ggplot2::element_rect(fill = grDevices::adjustcolor(k$fundo, .85), colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA), legend.title = ggplot2::element_text(size = ggplot2::rel(.95)))
}
