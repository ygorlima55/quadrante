# Quadrante: modo "Seca e chuva" (24/11/2026, pedido do usuário com o documento "Estrutura seca e chuva").
# A partir da planilha juntada (coluna Campanha), sem separar:
#   1 e 2. cada campanha analisada sozinha: resumo, acumulação e estimadores, composição, distribuição por ponto,
#          fitofisionomia e método, diversidade por ponto e rarefação por fitofisionomia, ranking de abundância,
#          constância de Dajoz, NMDS com PERMANOVA/PERMDISP por fitofisionomia e UPGMA;
#   3. comparação: lado a lado, suficiência, diversidade (Hutcheson, Hill), estrutura, comparação pareada por ponto
#      (GLMM com efeito aleatório de ponto e Wilcoxon pareado), composição (NMDS, PERMANOVA e PERMDISP com
#      permutação restrita dentro do ponto, SIMPER, Baselga, UPGMA) e espécies indicadoras (IndVal).
# A unidade amostral é o ponto visitado na campanha. As análises gerais do resto do painel continuam com as campanhas somadas.

# ---- estações: ordem (chuva primeiro), nome e cor ----
tipo_estacao <- function(x) {
  s <- tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT")); s[is.na(s)] <- ""
  ifelse(grepl("chuv|umid|wet|cheia", s), "chuva", ifelse(grepl("seca|sec\\b|dry|estiag|^sec", s), "seca", "outra"))
}
estacoes_ordem <- function(reg, cods) {
  est <- vapply(cods, function(c) { v <- reg$estacao[reg$camp_cod == c]; v <- v[!is.na(v) & nzchar(v)]
    t <- if (length(v)) tipo_estacao(names(sort(table(v), decreasing = TRUE))[1]) else "outra"
    if (t == "outra") tipo_estacao(c) else t }, "")
  o <- order(match(est, c("chuva", "seca", "outra")), seq_along(cods))
  data.frame(codigo = cods[o], tipo = unname(est[o]),
    rotulo = ifelse(est[o] == "chuva", "Chuva", ifelse(est[o] == "seca", "Seca", cods[o])),
    subtitulo = ifelse(est[o] == "chuva", "Campanha chuvosa", ifelse(est[o] == "seca", "Campanha seca", paste("Campanha", cods[o]))),
    stringsAsFactors = FALSE)
}
misturar <- function(cor, f, com = "#ffffff") { a <- grDevices::col2rgb(cor) / 255; b <- grDevices::col2rgb(com) / 255; grDevices::rgb(t(a * (1 - f) + b * f)) }
claro <- function(cor) misturar(cor, .5)
escuro <- function(cor) misturar(cor, .2, "#000000")
cores_estacoes <- function(Ee, pal) {
  base <- ifelse(Ee$tipo == "chuva", pal[1], ifelse(Ee$tipo == "seca", pal[2], pal[(seq_len(nrow(Ee)) + 2 - 1) %% length(pal) + 1]))
  setNames(base, Ee$rotulo)
}

# ---- contas de uma campanha ----
div_vetor <- function(v) {
  v <- v[v > 0]; N <- sum(v); S <- length(v)
  if (!N) return(c(S = 0, N = 0, H = NA, simpson = NA, pielou = NA, bp = NA, hill1 = NA, hill2 = NA))
  p <- v / N; H <- -sum(p * log(p)); D <- sum(p^2)
  c(S = S, N = N, H = H, simpson = 1 - D, pielou = if (S > 1) H / log(S) else NA, bp = max(p), hill1 = exp(H), hill2 = 1 / D)
}
hill_q <- function(v, q) { v <- v[v > 0]; p <- v / sum(v); if (abs(q - 1) < 1e-9) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q)) }
dajoz <- function(pa) {
  if (is.null(pa) || !nrow(pa)) return(NULL)
  f <- 100 * colSums(pa > 0) / nrow(pa); f <- f[f > 0]
  data.frame(especie = names(f), frequencia = round(as.numeric(f), 1),
             categoria = ifelse(f >= 50, "Constante", ifelse(f >= 25, "Acessória", "Acidental")), stringsAsFactors = FALSE)
}
embrulha <- function(x, n = 16) vapply(as.character(x), function(s) paste(strwrap(s, n), collapse = "\n"), "", USE.NAMES = FALSE)
quebra_familia <- function(x) ifelse(nchar(x) > 10 & grepl("idae$", x), sub("idae$", "-\nidae", x), x)
# nome da espécie abreviado numa linha só ("P. mystacalis"): a figura alarga para caber (pedido do usuário, 24/11/2026)
abrev_quebra <- function(x) { p <- strsplit(x, " "); vapply(p, function(s) if (length(s) >= 2) paste0(substr(s[1], 1, 1), ". ", paste(s[-1], collapse = " ")) else s[1], "") }
larg_especies <- function(n, w) max(w, 1.05 * n + 1)
matriz_ab <- function(r, pts) {
  if (!nrow(r)) return(matrix(0, length(pts), 0, dimnames = list(pts, NULL)))
  m <- tapply(r$total, list(factor(r$ponto_campo, levels = pts), r$especie), sum); m[is.na(m)] <- 0; as.matrix(m)
}
hc_simples <- function(hc) list(merge = hc$merge, height = hc$height, order = hc$order, labels = hc$labels)   # sem a chamada do R: vai para o dados.js
ordena_pontos <- function(p) p[order(suppressWarnings(as.integer(sub("\\D+", "", p))), p)]

uma_campanha <- function(rc, pts, pontos_info, grupos_reg) {
  ab <- matriz_ab(rc, pts); pa <- (ab > 0) * 1; tot <- colSums(ab)
  dv <- div_vetor(tot)
  out <- list(pontos = pts, ab = ab, resumo = as.list(dv), familias_n = length(unique(rc$familia)))
  out$resumo$familias <- out$familias_n
  out$resumo$dominante <- if (length(tot)) names(which.max(tot)) else NA_character_
  # 2. acumulação e estimadores (pontos da campanha como unidades)
  if (nrow(pa) >= 2 && ncol(pa) >= 1) {
    sa <- tryCatch(vegan::specaccum(pa, method = "random", permutations = 199), error = function(e) NULL)
    if (!is.null(sa)) out$curva <- data.frame(pontos = sa$sites, riqueza = sa$richness, li = pmax(0, sa$richness - 1.96 * sa$sd), ls = sa$richness + 1.96 * sa$sd)
    # curvas do Jackknife 1 e do Bootstrap ponto a ponto (poolaccum), desenhadas junto da observada (pedido do usuário, 24/11/2026)
    if (nrow(pa) >= 3) {
      pl <- tryCatch(summary(vegan::poolaccum(pa, permutations = 199, minsize = 2), display = c("jack1", "boot")), error = function(e) NULL)
      if (!is.null(pl)) out$curva_est <- rbind(data.frame(estimador = "Jackknife 1", pontos = pl$jack1[, 1], valor = pl$jack1[, 2]),
                                               data.frame(estimador = "Bootstrap", pontos = pl$boot[, 1], valor = pl$boot[, 2]))
    }
    sp <- tryCatch(vegan::specpool(pa), error = function(e) NULL)
    if (!is.null(sp)) out$est <- data.frame(estimador = c("Observada", "Chao 2", "Jackknife 1", "Jackknife 2", "Bootstrap"),
      valor = c(sp$Species, sp$chao, sp$jack1, sp$jack2, sp$boot), ep = c(NA, sp$chao.se, sp$jack1.se, NA, sp$boot.se), stringsAsFactors = FALSE)
    if (!is.null(out$est)) out$resumo$completude <- dv[["S"]] / max(out$est$valor, na.rm = TRUE)
  }
  # 3. composição: espécies por família (com o grupo) e as mais abundantes
  if (nrow(rc)) {
    fg <- unique(rc[, c("especie", "familia", "grupo_pad")]); fg <- fg[!duplicated(fg$especie), ]
    fam <- aggregate(especie ~ familia + grupo_pad, data = fg, FUN = length); names(fam)[3] <- "S"
    out$familias <- fam[order(-fam$S, fam$familia), ]
    o <- order(-tot); out$abundantes <- data.frame(especie = names(tot)[o], n = as.numeric(tot[o]), relativa = as.numeric(tot[o]) / sum(tot), stringsAsFactors = FALSE)
  }
  # 4 e 5. por ponto: riqueza, abundância, Shannon e Pielou
  out$por_ponto <- data.frame(ponto = pts, S = rowSums(pa), N = rowSums(ab),
    H = apply(ab, 1, function(v) abs(div_vetor(v)[["H"]])), J = apply(ab, 1, function(v) div_vetor(v)[["pielou"]]), stringsAsFactors = FALSE)
  # fitofisionomia de cada ponto
  fito <- if (!is.null(pontos_info) && "fitofisionomia" %in% names(pontos_info)) pontos_info$fitofisionomia[match(pts, pontos_info$ponto)] else rep(NA_character_, length(pts))
  out$fito_ponto <- setNames(fito, pts)
  okf <- !is.na(fito) & nzchar(fito)
  if (sum(okf) >= 2 && length(unique(fito[okf])) >= 2) {
    cl <- unique(fito[okf]); cl <- cl[order(match(cl, unique(fito[okf])))]
    vf <- lapply(setNames(cl, cl), function(f) colSums(ab[fito == f & okf, , drop = FALSE]))
    nmin <- min(vapply(vf, sum, 1)[vapply(vf, sum, 1) > 0])
    out$fito <- do.call(rbind, lapply(cl, function(f) { v <- vf[[f]]; d <- div_vetor(v)
      data.frame(fitofisionomia = f, N = d[["N"]], S = d[["S"]], rarefeita = if (d[["N"]] >= nmin && d[["N"]] > 0) as.numeric(vegan::rarefy(v[v > 0], nmin)) else NA,
                 H = d[["H"]], J = d[["pielou"]], stringsAsFactors = FALSE) }))
    out$fito_nmin <- nmin
    out$fito_curvas <- do.call(rbind, lapply(cl, function(f) { v <- vf[[f]]; v <- v[v > 0]; if (sum(v) < 2) return(NULL)
      n <- unique(round(seq(1, sum(v), length.out = 60))); data.frame(fitofisionomia = f, n = n, S = as.numeric(vegan::rarefy(v, n)), stringsAsFactors = FALSE) }))
  }
  # método de registro (do registro ou, se vazio, da aba de pontos)
  met <- rc$metodo; if (is.null(met)) met <- rep(NA_character_, nrow(rc))
  if (!is.null(pontos_info) && "metodo" %in% names(pontos_info)) { f <- is.na(met) | !nzchar(met); met[f] <- pontos_info$metodo[match(rc$ponto_campo[f], pontos_info$ponto)] }
  okm <- !is.na(met) & nzchar(met)
  if (any(okm) && length(unique(met[okm])) >= 2) out$metodo <- do.call(rbind, lapply(unique(met[okm]), function(mt) { q <- rc[okm & met == mt, ]
    data.frame(metodo = mt, S = length(unique(q$especie)), N = sum(q$total), stringsAsFactors = FALSE) }))
  # 6. estrutura
  out$dajoz <- dajoz(pa)
  # 7. similaridade entre os pontos com registro
  mm <- ab[rowSums(ab) > 0, colSums(ab) > 0, drop = FALSE]
  if (nrow(mm) >= 3 && ncol(mm) >= 2) {
    dj <- vegan::vegdist((mm > 0) * 1, "jaccard"); hc <- stats::hclust(dj, "average")
    out$upgma <- list(hc = hc_simples(hc), cofenetica = suppressWarnings(stats::cor(stats::cophenetic(hc), dj)), fito = out$fito_ponto[rownames(mm)])
    if (nrow(mm) >= 4) {
      db <- vegan::vegdist(mm, "bray")
      nm <- tryCatch(suppressWarnings(vegan::metaMDS(db, k = 2, trymax = 50, trace = 0)), error = function(e) NULL)
      if (!is.null(nm)) out$nmds <- list(pontos = data.frame(ponto = rownames(mm), x = nm$points[, 1], y = nm$points[, 2], fito = out$fito_ponto[rownames(mm)], stringsAsFactors = FALSE), stress = nm$stress)
      ff <- out$fito_ponto[rownames(mm)]
      if (all(!is.na(ff) & nzchar(ff)) && length(unique(ff)) >= 2 && all(table(ff) >= 2)) {
        ad <- tryCatch(vegan::adonis2(db ~ ff, permutations = 999), error = function(e) NULL)
        if (!is.null(ad)) out$permanova <- list(F = ad$F[1], R2 = ad$R2[1], p = ad$`Pr(>F)`[1])
        bd <- tryCatch(vegan::permutest(vegan::betadisper(db, ff), permutations = 999), error = function(e) NULL)
        if (!is.null(bd)) out$permdisp <- list(F = bd$tab$F[1], p = bd$tab$`Pr(>F)`[1])
      }
    }
  }
  out
}

# ---- IndVal (Dufrêne e Legendre 1997) com permutação dentro do ponto ----
indval_calc <- function(m, g) {
  gs <- unique(g); medias <- sapply(gs, function(k) colMeans(m[g == k, , drop = FALSE])); if (is.null(dim(medias))) medias <- matrix(medias, nrow = 1, dimnames = list(colnames(m), gs))
  A <- medias / pmax(rowSums(medias), 1e-12)
  B <- sapply(gs, function(k) colMeans(m[g == k, , drop = FALSE] > 0)); if (is.null(dim(B))) B <- matrix(B, nrow = 1, dimnames = list(colnames(m), gs))
  iv <- A * B; k <- max.col(iv, ties.method = "first")
  data.frame(especie = colnames(m), grupo = gs[k], A = A[cbind(seq_len(nrow(A)), k)], B = B[cbind(seq_len(nrow(B)), k)], indval = iv[cbind(seq_len(nrow(iv)), k)], stringsAsFactors = FALSE)
}
indval_blocos <- function(m, g, blocos, nperm = 999) {
  obs <- indval_calc(m, g); maior <- numeric(nrow(obs))
  ctrl <- permute::how(blocks = factor(blocos), nperm = nperm)
  for (i in seq_len(nperm)) { gp <- g[permute::shuffle(length(g), ctrl)]; maior <- maior + (indval_calc(m, gp)$indval >= obs$indval - 1e-12) }
  obs$p <- (maior + 1) / (nperm + 1); obs
}

# ---- comparação entre as campanhas ----
analisar_estacoes <- function(reg, excluidas, campanhas, pontos_info, nome) {
  if (is.null(campanhas) || is.null(reg$camp_cod)) return(NULL)
  vc <- attr(campanhas, "visitas")
  visitas <- unique(reg[c("ponto_campo", "camp_cod")])
  if (!is.null(vc) && nrow(vc)) visitas <- unique(rbind(visitas, data.frame(ponto_campo = vc$ponto, camp_cod = campanhas$codigo[match(vc$campanha, campanhas$campanha)])))
  visitas <- visitas[!is.na(visitas$camp_cod), ]
  r <- reg[!(reg$especie %in% excluidas) & !is.na(reg$total) & reg$total > 0, ]
  r$familia[is.na(r$familia) | !nzchar(r$familia)] <- "Família não informada"
  cods <- campanhas$codigo[campanhas$codigo %in% visitas$camp_cod]
  if (length(cods) < 2 || !nrow(r)) return(NULL)
  Ee <- estacoes_ordem(r, cods); cods <- Ee$codigo
  camp <- lapply(setNames(cods, Ee$rotulo), function(c) {
    pts <- ordena_pontos(unique(c(visitas$ponto_campo[visitas$camp_cod == c], r$ponto_campo[r$camp_cod == c])))
    uma_campanha(r[r$camp_cod == c, , drop = FALSE], pts, pontos_info)
  })
  rot <- Ee$rotulo
  C <- list(nome = nome, estacoes = Ee, campanhas = camp)
  # resumo e estimadores lado a lado
  C$resumo <- do.call(rbind, lapply(rot, function(s) { q <- camp[[s]]$resumo
    data.frame(estacao = s, riqueza = q$S, abundancia = q$N, familias = q$familias, shannon = q$H, simpson = q$simpson, pielou = q$pielou,
               berger_parker = q$bp, hill1 = q$hill1, hill2 = q$hill2, completude = q$completude %||% NA_real_, dominante = q$dominante, pontos = length(camp[[s]]$pontos), stringsAsFactors = FALSE) }))
  C$estimadores <- do.call(rbind, lapply(rot, function(s) if (!is.null(camp[[s]]$est)) cbind(estacao = s, camp[[s]]$est)))
  # 1. lado a lado: grupos (total e cada grupo da coluna Grupo), fitofisionomias e métodos
  ladol <- function(chave, valores, filtro) do.call(rbind, lapply(seq_along(cods), function(i) do.call(rbind, lapply(valores, function(v) {
    q <- r[r$camp_cod == cods[i] & filtro(v), , drop = FALSE]
    data.frame(estacao = rot[i], categoria = v, S = length(unique(q$especie)), N = sum(q$total), stringsAsFactors = FALSE) }))))
  gp <- unique(r$grupo_pad); gp <- gp[order(match(gp, c("Anuros", "Répteis")), gp)]
  # só o grupo desta análise: Herpetofauna = todas as espécies; Anuros e Répteis têm cada um as suas figuras (pedido do usuário, 24/11/2026)
  C$grupos <- ladol("grupo", nome, function(v) rep(TRUE, nrow(r)))
  fitos <- unique(unlist(lapply(camp, function(x) x$fito$fitofisionomia)))
  if (length(fitos) >= 2) {
    fp <- if (!is.null(pontos_info)) pontos_info$fitofisionomia[match(r$ponto_campo, pontos_info$ponto)] else NA
    C$fito <- ladol("fito", fitos, function(v) !is.na(fp) & fp == v)
  }
  mets <- unique(unlist(lapply(camp, function(x) x$metodo$metodo)))
  if (length(mets) >= 2) C$metodo <- do.call(rbind, lapply(rot, function(s) { m <- camp[[s]]$metodo; if (is.null(m)) return(NULL)
    m <- m[match(mets, m$metodo), ]; m$metodo <- mets; m$S[is.na(m$S)] <- 0; m$N[is.na(m$N)] <- 0; data.frame(estacao = s, categoria = m$metodo, S = m$S, N = m$N, stringsAsFactors = FALSE) }))
  todos_pts <- ordena_pontos(unique(visitas$ponto_campo))
  C$pontos <- do.call(rbind, lapply(rot, function(s) { pp <- camp[[s]]$por_ponto
    data.frame(estacao = s, categoria = todos_pts, S = pp$S[match(todos_pts, pp$ponto)], N = pp$N[match(todos_pts, pp$ponto)], stringsAsFactors = FALSE) }))
  # ponto sem nenhum registro (em nenhuma campanha) não aparece (pedido do usuário, 24/11/2026)
  com_reg <- unique(C$pontos$categoria[!is.na(C$pontos$S) & C$pontos$S > 0])
  C$pontos <- C$pontos[C$pontos$categoria %in% com_reg, , drop = FALSE]
  # 2. rarefação e extrapolação (iNEXT, q = 0) de cada estação, no total e em cada grupo
  rar <- tryCatch(lapply(nome, function(g) {
    rr <- if (g == nome) r else r[r$grupo_pad == g, ]
    vs <- lapply(cods, function(c) { v <- tapply(rr$total[rr$camp_cod == c], rr$especie[rr$camp_cod == c], sum); v <- sort(v[!is.na(v) & v > 0], decreasing = TRUE); as.numeric(v) }); names(vs) <- rot
    vs <- vs[vapply(vs, function(v) length(v) >= 2 && sum(v) >= 4, TRUE)]; if (!length(vs)) return(NULL)
    o <- suppressWarnings(iNEXT::iNEXT(vs, q = 0, datatype = "abundance", endpoint = 2 * max(vapply(vs, sum, 1)), knots = 40, nboot = 100))
    z <- o$iNextEst$size_based
    tab <- do.call(rbind, lapply(names(vs), function(s) { v <- vs[[s]]; n_min <- min(vapply(vs, sum, 1))
      ch1 <- tryCatch(o$AsyEst$Estimator[o$AsyEst$Assemblage == s & o$AsyEst$Diversity == "Species richness"], error = function(e) NA)
      data.frame(grupo = g, estacao = s, N = sum(v), S = length(v), chao1 = if (length(ch1)) ch1[1] else NA, cobertura = 1 - sum(v == 1) / sum(v) * ((sum(v) - 1) * sum(v == 1) / ((sum(v) - 1) * sum(v == 1) + 2 * max(sum(v == 2), 1))),
                 rarefeita = as.numeric(vegan::rarefy(v, n_min)), n_rarefeita = n_min, stringsAsFactors = FALSE) }))
    list(tab = tab, curvas = data.frame(grupo = g, estacao = as.character(z$Assemblage), m = z$m, metodo = as.character(z$Method), qD = z$qD, li = z$qD.LCL, ls = z$qD.UCL, stringsAsFactors = FALSE)) }),
    error = function(e) { message("estações, rarefação: ", conditionMessage(e)); NULL })
  rar <- Filter(Negate(is.null), rar)
  if (length(rar)) { C$rarefacao <- do.call(rbind, lapply(rar, `[[`, "tab")); C$rarefacao_curvas <- do.call(rbind, lapply(rar, `[[`, "curvas")) }
  # 3. diversidade: perfil de Hill e Hutcheson
  tots <- lapply(camp, function(x) colSums(x$ab))
  qs <- seq(0, 3, by = .05)
  C$perfil <- do.call(rbind, lapply(rot, function(s) data.frame(estacao = s, q = qs, D = vapply(qs, function(q) hill_q(tots[[s]], q), 1))))
  mc <- do.call(rbind, lapply(rot, function(s) { v <- tots[[s]]; u <- unique(unlist(lapply(tots, names))); x <- setNames(numeric(length(u)), u); x[names(v)] <- v; x })); rownames(mc) <- rot
  C$hutcheson <- tryCatch({ h <- hutcheson(mc); p <- h$pares; names(p)[1:4] <- c("estacao_a", "estacao_b", "H_a", "H_b"); p }, error = function(e) NULL)
  # 4. ranking e Dajoz das duas
  C$ranking <- do.call(rbind, lapply(rot, function(s) { a <- camp[[s]]$abundantes; if (is.null(a)) NULL else cbind(estacao = s, posicao = seq_len(nrow(a)), a) }))
  C$dajoz <- do.call(rbind, lapply(rot, function(s) { d <- camp[[s]]$dajoz; if (is.null(d)) NULL else cbind(estacao = s, d) }))
  # unidades ponto × campanha (só as visitadas)
  un <- visitas[visitas$camp_cod %in% cods, ]; un$estacao <- rot[match(un$camp_cod, cods)]
  un <- un[order(match(un$estacao, rot), match(un$ponto_campo, todos_pts)), ]
  mu <- do.call(rbind, lapply(seq_len(nrow(un)), function(i) { x <- camp[[un$estacao[i]]]$ab; v <- if (un$ponto_campo[i] %in% rownames(x)) x[un$ponto_campo[i], ] else numeric(0)
    u <- colnames(mc); y <- setNames(numeric(length(u)), u); y[names(v)] <- v; y }))
  rownames(mu) <- paste(un$ponto_campo, un$estacao, sep = " · ")
  un$fito <- if (!is.null(pontos_info)) pontos_info$fitofisionomia[match(un$ponto_campo, pontos_info$ponto)] else NA
  C$unidades <- un
  # 5. comparação pareada por ponto (pontos visitados nas duas primeiras campanhas)
  if (length(rot) >= 2) {
    a <- rot[1]; b <- rot[2]; pa_ <- camp[[a]]$por_ponto; pb_ <- camp[[b]]$por_ponto
    comuns <- intersect(pa_$ponto, pb_$ponto)
    par <- data.frame(ponto = comuns, S_a = pa_$S[match(comuns, pa_$ponto)], S_b = pb_$S[match(comuns, pb_$ponto)], N_a = pa_$N[match(comuns, pa_$ponto)], N_b = pb_$N[match(comuns, pb_$ponto)])
    par <- par[par$S_a + par$S_b > 0, , drop = FALSE]   # ponto sem registro nas duas não entra
    C$pareada_pontos <- par
    if (nrow(par) >= 4) {
      teste <- function(va, vb, fam) {
        w <- tryCatch(suppressWarnings(stats::wilcox.test(va, vb, paired = TRUE, exact = FALSE))$p.value, error = function(e) NA)
        d <- data.frame(y = c(va, vb), estacao = factor(rep(c(a, b), each = length(va)), levels = c(a, b)), ponto = factor(rep(par$ponto, 2)))
        g <- NULL
        if (requireNamespace("glmmTMB", quietly = TRUE)) {
          for (fm in fam) { g <- tryCatch(suppressWarnings(glmmTMB::glmmTMB(y ~ estacao + (1 | ponto), data = d, family = fm)), error = function(e) NULL)
            if (!is.null(g) && isTRUE(g$sdr$pdHess)) break else g <- NULL }
        }
        res <- data.frame(media_a = mean(va), media_b = mean(vb), razao = NA, li = NA, ls = NA, p_glmm = NA, p_wilcoxon = w, modelo = NA_character_)
        if (!is.null(g)) { cf <- summary(g)$coefficients$cond; k <- grep("^estacao", rownames(cf))[1]
          est <- cf[k, "Estimate"]; se <- cf[k, "Std. Error"]
          res$razao <- exp(est); res$li <- exp(est - 1.96 * se); res$ls <- exp(est + 1.96 * se); res$p_glmm <- cf[k, "Pr(>|z|)"]
          res$modelo <- g$modelInfo$family$family }
        res }
      tem_tmb <- requireNamespace("glmmTMB", quietly = TRUE)   # sem o glmmTMB fica só o Wilcoxon pareado
      C$pareada <- rbind(cbind(variavel = "Riqueza por ponto", teste(par$S_a, par$S_b, if (tem_tmb) list(stats::poisson(), glmmTMB::nbinom2()) else list())),
                         cbind(variavel = "Abundância por ponto", teste(par$N_a, par$N_b, if (tem_tmb) list(glmmTMB::nbinom2(), stats::poisson()) else list())))
    }
  }
  # 6. composição: NMDS, PERMANOVA e PERMDISP com permutação restrita, SIMPER, Baselga, UPGMA
  ok <- rowSums(mu) > 0; mu2 <- mu[ok, colSums(mu[ok, , drop = FALSE]) > 0, drop = FALSE]; un2 <- un[ok, ]
  if (nrow(mu2) >= 4 && ncol(mu2) >= 2) {
    db <- vegan::vegdist(mu2, "bray"); est <- factor(un2$estacao, levels = rot); pt <- factor(un2$ponto_campo)
    nm <- tryCatch(suppressWarnings(vegan::metaMDS(db, k = 2, trymax = 50, trace = 0)), error = function(e) NULL)
    if (!is.null(nm)) C$nmds <- list(pontos = data.frame(unidade = rownames(mu2), ponto = un2$ponto_campo, estacao = un2$estacao, fito = un2$fito, x = nm$points[, 1], y = nm$points[, 2], stringsAsFactors = FALSE), stress = nm$stress)
    bloco <- any(duplicated(pt))
    ctrl <- if (bloco) permute::how(blocks = pt, nperm = 999) else permute::how(nperm = 999)
    pm <- list()
    a1 <- tryCatch(vegan::adonis2(db ~ est, permutations = ctrl), error = function(e) NULL)
    if (!is.null(a1)) pm[[length(pm) + 1]] <- data.frame(teste = "PERMANOVA · estação", gl = a1$Df[1], F = a1$F[1], R2 = a1$R2[1], p = a1$`Pr(>F)`[1])
    ff <- factor(un2$fito)
    if (all(!is.na(un2$fito)) && nlevels(ff) >= 2) {
      cf <- if (bloco) permute::how(plots = permute::Plots(strata = pt, type = "free"), within = permute::Within(type = "none"), nperm = 999) else permute::how(nperm = 999)
      a2 <- tryCatch(vegan::adonis2(db ~ ff, permutations = cf), error = function(e) NULL)
      if (!is.null(a2)) pm[[length(pm) + 1]] <- data.frame(teste = "PERMANOVA · fitofisionomia", gl = a2$Df[1], F = a2$F[1], R2 = a2$R2[1], p = a2$`Pr(>F)`[1])
      a3 <- tryCatch(vegan::adonis2(db ~ est * ff, permutations = ctrl, by = "terms"), error = function(e) NULL)
      if (!is.null(a3) && nrow(a3) >= 4) pm[[length(pm) + 1]] <- data.frame(teste = "PERMANOVA · estação × fitofisionomia", gl = a3$Df[3], F = a3$F[3], R2 = a3$R2[3], p = a3$`Pr(>F)`[3])
    }
    bd <- tryCatch(vegan::permutest(vegan::betadisper(db, est), permutations = ctrl), error = function(e) NULL)
    if (!is.null(bd)) pm[[length(pm) + 1]] <- data.frame(teste = "PERMDISP · estação", gl = bd$tab$Df[1], F = bd$tab$F[1], R2 = NA, p = bd$tab$`Pr(>F)`[1])
    C$permanova <- if (length(pm)) do.call(rbind, pm)
    C$permutacao <- if (bloco) "restrita dentro do ponto (blocos); fitofisionomia permuta pontos inteiros" else "livre (nenhum ponto foi visitado nas duas campanhas)"
    sp <- tryCatch(summary(vegan::simper(mu2, est, permutations = 0))[[1]], error = function(e) NULL)
    if (!is.null(sp)) { sp <- head(sp[order(-sp$average), ], 8); tot_d <- tryCatch(sum(summary(vegan::simper(mu2, est, permutations = 0))[[1]]$average), error = function(e) NA)
      C$simper <- data.frame(especie = rownames(sp), contribuicao = sp$average / tot_d, acumulada = cumsum(sp$average) / tot_d,
        mais_em = ifelse(sp$ava >= sp$avb, rot[1], rot[2]), stringsAsFactors = FALSE) }
    hc <- stats::hclust(vegan::vegdist((mu2 > 0) * 1, "jaccard"), "average")
    C$upgma <- list(hc = hc_simples(hc), cofenetica = suppressWarnings(stats::cor(stats::cophenetic(hc), vegan::vegdist((mu2 > 0) * 1, "jaccard"))), estacao = setNames(un2$estacao, rownames(mu2)), ponto = setNames(un2$ponto_campo, rownames(mu2)))
  }
  # Baselga: lista total da área e média dos pontos visitados nas duas
  if (length(rot) >= 2) {
    pr <- (mc[rot[1:2], , drop = FALSE] > 0) * 1
    bt <- tryCatch(beta_baselga(pr), error = function(e) NULL)
    bp <- NULL
    if (!is.null(C$pareada_pontos) && nrow(C$pareada_pontos)) {
      bs <- lapply(C$pareada_pontos$ponto, function(p) { x <- (rbind(camp[[rot[1]]]$ab[p, ], camp[[rot[2]]]$ab[p, ]) > 0) * 1
        if (is.null(dim(x)) || sum(x) == 0 || any(rowSums(x) == 0)) return(NULL)
        tryCatch(beta_baselga(x[, colSums(x) > 0, drop = FALSE]), error = function(e) NULL) })
      bs <- Filter(Negate(is.null), bs)
      if (length(bs)) bp <- c(sorensen = mean(vapply(bs, `[[`, 1, "sorensen")), turnover = mean(vapply(bs, `[[`, 1, "turnover")), aninhamento = mean(vapply(bs, `[[`, 1, "aninhamento")))
    }
    C$baselga <- rbind(if (!is.null(bt)) data.frame(escala = "Lista total da área", sorensen = bt$sorensen, substituicao = bt$turnover, aninhamento = bt$aninhamento),
                       if (!is.null(bp)) data.frame(escala = "Média dos pontos", sorensen = bp[["sorensen"]], substituicao = bp[["turnover"]], aninhamento = bp[["aninhamento"]]))
  }
  # 7. IndVal com permutação dentro do ponto
  if (exists("mu2") && nrow(mu2) >= 4) {
    iv <- tryCatch(indval_blocos(mu2, un2$estacao, un2$ponto_campo, 999), error = function(e) { message("IndVal: ", conditionMessage(e)); NULL })
    if (!is.null(iv)) { iv <- iv[iv$p < .05 & iv$B > 0, ]; C$indval <- iv[order(match(iv$grupo, rot), -iv$indval), ] }
  }
  C$texto <- textos_estacoes(C)
  C
}

# ---- Excel: tabelas/10_<grupo>_seca_e_chuva.xlsx ----
exportar_estacoes <- function(R, saida) {
  for (g in names(R)) { C <- R[[g]]$estacoes; if (is.null(C)) next
    abas <- list(`Resumo` = C$resumo, `Estimadores` = C$estimadores, `Lado a lado - pontos` = C$pontos, `Lado a lado - grupos` = C$grupos,
      `Lado a lado - fito` = C$fito, `Lado a lado - metodo` = C$metodo, `Rarefacao iNEXT` = C$rarefacao, `Hutcheson` = C$hutcheson,
      `Ranking` = C$ranking, `Dajoz` = C$dajoz, `Pareada por ponto` = C$pareada_pontos, `GLMM e Wilcoxon` = C$pareada,
      `PERMANOVA e PERMDISP` = C$permanova, `SIMPER` = C$simper, `Baselga` = C$baselga, `IndVal` = C$indval)
    for (s in names(C$campanhas)) { X <- C$campanhas[[s]]
      abas[[paste(s, "- por ponto")]] <- X$por_ponto; abas[[paste(s, "- familias")]] <- X$familias; abas[[paste(s, "- fito")]] <- X$fito }
    abas <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, abas)
    if (length(abas)) writexl::write_xlsx(abas, file.path(saida, "tabelas", sprintf("10_%s_seca_e_chuva.xlsx", tag_grupo(g))))
  }
}

# ---- textos para o relatório ----
textos_estacoes <- function(C) {
  f <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d, decimal.mark = ","), "–")
  pv <- function(p) ifelse(!is.finite(p), "–", ifelse(p < .001, "p < 0,001", paste0("p = ", f(p, 3))))
  R <- C$resumo; out <- list()
  out$resumo <- paste(vapply(seq_len(nrow(R)), function(i) sprintf("Na %s foram registradas %d espécies de %d famílias, com %s indivíduos em %d pontos (H' = %s; J' = %s); a espécie mais abundante foi %s.",
    tolower(C$estacoes$subtitulo[i]), as.integer(R$riqueza[i]), as.integer(R$familias[i]), format(R$abundancia[i], big.mark = "."), as.integer(R$pontos[i]), f(R$shannon[i]), f(R$pielou[i]), R$dominante[i]), ""), collapse = " ")
  if (!is.null(C$estimadores)) out$suficiencia <- paste(vapply(C$estacoes$rotulo, function(s) { q <- C$estimadores[C$estimadores$estacao == s, ]
    sprintf("%s: observada %d; Chao 2 %s, Jackknife 1 %s, Jackknife 2 %s e Bootstrap %s (completude de %s%%).", s, as.integer(q$valor[1]), f(q$valor[2], 1), f(q$valor[3], 1), f(q$valor[4], 1), f(q$valor[5], 1),
            f(100 * C$resumo$completude[C$resumo$estacao == s], 0)) }, ""), collapse = " ")
  if (!is.null(C$hutcheson) && nrow(C$hutcheson)) { h <- C$hutcheson[1, ]
    out$diversidade <- sprintf("O teste t de Hutcheson comparou a diversidade de Shannon entre %s (H' = %s) e %s (H' = %s): t = %s, gl = %s, %s.", h$estacao_a, f(h$H_a), h$estacao_b, f(h$H_b), f(h$t), f(h$gl, 0), pv(h$p)) }
  if (!is.null(C$pareada)) { p <- C$pareada
    out$pareada <- paste(vapply(seq_len(nrow(p)), function(i) sprintf("%s: média de %s na %s e %s na %s (razão %s/%s = %s; IC 95%% %s a %s; GLMM %s; Wilcoxon pareado %s).",
      p$variavel[i], f(p$media_a[i], 1), tolower(C$estacoes$rotulo[1]), f(p$media_b[i], 1), tolower(C$estacoes$rotulo[2]), tolower(C$estacoes$rotulo[2]), tolower(C$estacoes$rotulo[1]),
      f(p$razao[i]), f(p$li[i]), f(p$ls[i]), pv(p$p_glmm[i]), pv(p$p_wilcoxon[i])), ""), collapse = " ") }
  if (!is.null(C$permanova)) { pm <- C$permanova
    out$composicao <- paste0(paste(sprintf("%s: F = %s, %s%s.", pm$teste, f(pm$F), ifelse(is.finite(pm$R2), paste0("R² = ", f(pm$R2), ", "), ""), pv(pm$p)), collapse = " "),
      " Permutações ", C$permutacao, ".", if (any(grepl("PERMDISP", pm$teste) & pm$p < .05)) " A PERMDISP foi significativa: parte da diferença da PERMANOVA pode ser de dispersão, e não só de composição." else "") }
  if (!is.null(C$indval) && nrow(C$indval)) out$indval <- paste0("Espécies indicadoras (IndVal, p < 0,05): ", paste(sprintf("%s (%s, %s)", C$indval$especie, C$indval$grupo, f(100 * C$indval$indval, 1)), collapse = "; "), ".")
  out$cuidados <- "Com uma campanha por estação, o efeito da estação se confunde com o da campanha (ano, clima daqueles dias, equipe). Escreva \"registrada apenas na seca\", não \"ocorre apenas na seca\". Totais só se comparam com esforço igual; se diferir, compare pela rarefação."
  out
}

# ---- figuras ----
catalogo_est <- function() {
  um <- list(acumulacao = "Curva de acumulação de espécies", familias = "Riqueza por família", abundantes = "Espécies mais abundantes", pontos = "Riqueza e abundância por ponto",
             fito = "Riqueza e abundância por fitofisionomia", metodo = "Riqueza e abundância por método", diversidade = "Diversidade por ponto", rarefacao_fito = "Rarefação por fitofisionomia",
             ranking = "Curva de ranking de abundância", dajoz = "Constância de ocorrência (Dajoz)", nmds = "Ordenação NMDS dos pontos", upgma = "Agrupamento dos pontos (UPGMA)")
  cp <- list(pontos = "Riqueza e abundância por ponto", grupos = "Riqueza e abundância por estação", fito = "Riqueza e abundância por fitofisionomia", metodo = "Riqueza e abundância por método",
             acumulacao = "Curva de acumulação de espécies", estimadores = "Riqueza observada e estimada", rarefacao = "Curvas de rarefação e extrapolação", indices = "Índices de diversidade",
             hill = "Números de Hill (espécies efetivas)", perfil = "Perfil de diversidade", ranking = "Curva de ranking de abundância", dajoz = "Constância de ocorrência (Dajoz)",
             pareada = "Riqueza e abundância por ponto (pareada)", nmds = "Ordenação NMDS (Bray-Curtis)", simper = "Espécies que mais diferenciam as estações (SIMPER)", baselga = "Diversidade beta entre estações",
             upgma = "Agrupamento das unidades (UPGMA)", indval = "Espécies indicadoras (IndVal)")
  out <- list()
  for (i in 1:4) for (k in names(um)) out[[sprintf("est%d_%s", i, k)]] <- list(nome = paste0("Seca e chuva, campanha ", i, ": ", um[[k]]), padrao = "modelo", formas = list(modelo = um[[k]]))
  for (k in names(cp)) out[[paste0("estc_", k)]] <- list(nome = paste0("Seca e chuva, comparação: ", cp[[k]]), padrao = "modelo", formas = list(modelo = cp[[k]]))
  out
}

desenhar_est <- function(chave, forma, G, e, gg, pal, k, w, h) {
  C <- G$estacoes; if (is.null(C)) return(invisible())
  fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  tam <- e$fonte / 3.3; fam <- e$familia; Ee <- C$estacoes; rot <- Ee$rotulo
  cs <- cores_estacoes(Ee, pal)
  sub_ <- function(s) ggplot2::labs(subtitle = if (isTRUE(e$mostrar_titulo)) s else NULL)
  rotulo_val <- function(..., d = 0) ggplot2::geom_text(..., family = fam, fontface = "bold", size = tam * .78, colour = k$tinta, vjust = -.5, na.rm = TRUE)
  y0 <- ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12)))
  sem_y <- ggplot2::theme(axis.text.y = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank())
  tt <- function(ch, titulo, x = NULL, y = NULL, nota = NULL) titulos_gg(e, ch, titulo, x, y, nota)
  # legenda das fitofisionomias: letra menor, duas colunas, nomes com até ~34 letras por linha (antes saía enorme e cortada)
  leg_fito <- function(aes_ = "colour") list(do.call(ggplot2::guides, setNames(list(ggplot2::guide_legend(ncol = 2, byrow = TRUE)), aes_)),
    ggplot2::theme(legend.text = ggplot2::element_text(size = ggplot2::rel(.66), lineheight = .9), legend.key.height = grid::unit(.95, "lines"),
                   legend.spacing.y = grid::unit(.15, "lines"), legend.margin = ggplot2::margin(t = 2)))
  # legenda em dois blocos (Riqueza / Abundância de cada estação) com o nome da estação embaixo
  legenda_blocos <- function(cores_par) {
    n <- length(cores_par); xs <- 0; d <- NULL; seg <- NULL; nm <- NULL
    for (i in seq_len(n)) { x0 <- (i - 1) * 3.2
      d <- rbind(d, data.frame(x = x0 + c(0, 1.1), y = 1, cor = c(claro(cores_par[i]), escuro(cores_par[i])), txt = c("Riqueza", "Abundância")))
      seg <- rbind(seg, data.frame(x = x0 - .2, xend = x0 + 2.5, y = .55)); nm <- rbind(nm, data.frame(x = x0 + 1.15, y = .2, txt = names(cores_par)[i])) }
    ggplot2::ggplot() + ggplot2::geom_point(data = d, ggplot2::aes(x, y), colour = d$cor, size = 3.2) +
      ggplot2::geom_text(data = d, ggplot2::aes(x + .14, y, label = txt), hjust = 0, family = if (grepl("^Inter", fam)) "Inter SemiBold" else fam, size = tam * .9, colour = k$tinta) +
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = x, xend = xend, y = y, yend = y), colour = k$grade, linewidth = .4) +
      ggplot2::geom_text(data = nm, ggplot2::aes(x, y, label = txt), family = if (grepl("^Inter", fam)) "Inter SemiBold" else fam, size = tam * .9, colour = k$tinta2) +
      ggplot2::coord_cartesian(xlim = c(-1.5, (n - 1) * 3.2 + 3.6), ylim = c(0, 1.3), clip = "off") + ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA))
  }
  # barras de riqueza (claro) e abundância (escuro): uma estação ou várias lado a lado
  barras_sn <- function(d, ch, titulo, quebra = FALSE, ww = w) {
    cat_ <- if (quebra) gsub(" ", "\n", d$categoria) else embrulha(d$categoria, 22); d$categoria <- factor(cat_, levels = unique(cat_))
    tx <- if (max(lengths(strsplit(cat_, "\n"))) > 2 || nlevels(d$categoria) > 6) .66 else .85   # nomes longos (fitofisionomias): letra menor
    ests <- unique(d$estacao); varias <- length(ests) > 1
    l <- rbind(data.frame(d[c("estacao", "categoria")], medida = "Riqueza", v = d$S), data.frame(d[c("estacao", "categoria")], medida = "Abundância", v = d$N))
    l$serie <- factor(paste(l$medida, l$estacao), levels = as.vector(t(outer(c("Riqueza", "Abundância"), ests, paste))))
    if (!varias) l$serie <- factor(l$medida, levels = c("Riqueza", "Abundância"))
    cores <- if (varias) setNames(as.vector(t(outer(c("Riqueza", "Abundância"), ests, function(m, s) ifelse(m == "Riqueza", vapply(cs[s], claro, ""), vapply(cs[s], escuro, ""))))), levels(l$serie))
             else setNames(c(claro(cs[[ests]]), escuro(cs[[ests]])), c("Riqueza", "Abundância"))
    p <- ggplot2::ggplot(l, ggplot2::aes(categoria, v, fill = serie)) + ggplot2::geom_col(position = ggplot2::position_dodge(.86, preserve = "single"), width = .82, na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(v), "", v), group = serie), position = ggplot2::position_dodge(.86), family = fam, fontface = "bold", size = tam * (if (nlevels(l$categoria) > 8) .55 else .75), colour = k$tinta, vjust = -.5, na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = cores) + y0 + tt(ch, titulo) + tema_gg(e) + sem_y + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", colour = k$tinta, size = ggplot2::rel(tx), lineheight = .9))
    if (!varias) return(gg(p, ww, h))
    p <- p + ggplot2::theme(legend.position = "none")
    gg(cowplot::plot_grid(p, legenda_blocos(cs[ests]), ncol = 1, rel_heights = c(1, .13)), ww, h)
  }
  m <- regmatches(chave, regexec("^est([0-9])_(.*)$", chave))[[1]]
  if (length(m)) {
    i <- as.integer(m[2]); kk <- m[3]; if (i > length(rot)) return(invisible())
    s <- rot[i]; X <- C$campanhas[[s]]; cor <- cs[[s]]; subt <- sub_(paste(C$nome, "·", Ee$subtitulo[i]))
    if (kk == "acumulacao" && !is.null(X$curva)) {
      # observada (com a faixa de 95%), Jackknife 1 e Bootstrap juntos, cada um com o valor final na ponta
      d <- X$curva
      l <- data.frame(estimador = "Observada", pontos = d$pontos, valor = d$riqueza)
      if (!is.null(X$curva_est)) l <- rbind(l, X$curva_est)
      niv <- intersect(c("Observada", "Jackknife 1", "Bootstrap"), unique(l$estimador)); l$estimador <- factor(l$estimador, levels = niv)
      cores <- c("Observada" = cor, "Jackknife 1" = escuro(cor), "Bootstrap" = misturar(cor, .45))[niv]
      tipos <- c("Observada" = 1, "Jackknife 1" = 2, "Bootstrap" = 4)[niv]
      fim <- do.call(rbind, lapply(split(l, l$estimador), function(q) q[which.max(q$pontos), ])); if (exists("espalha_rotulos")) fim$yl <- espalha_rotulos(fim$valor, diff(range(c(0, l$valor, d$ls), na.rm = TRUE)) * .055) else fim$yl <- fim$valor
      p <- ggplot2::ggplot(l, ggplot2::aes(pontos, valor, colour = estimador, linetype = estimador)) +
        ggplot2::geom_ribbon(data = d, ggplot2::aes(x = pontos, ymin = li, ymax = ls), inherit.aes = FALSE, fill = cor, alpha = .14) +
        ggplot2::geom_line(linewidth = e$linha * 1.2) + ggplot2::geom_point(data = fim, size = e$ponto, show.legend = FALSE) +
        ggplot2::geom_text(data = fim, ggplot2::aes(y = yl, label = fmt(valor)), hjust = -.35, family = fam, fontface = "bold", size = tam * .8, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = cores) + ggplot2::scale_linetype_manual(values = tipos) +
        ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .1))) + ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 1)), linetype = ggplot2::guide_legend()) +
        tt(chave, "Curva de Acumulação de Espécies", "Pontos acumulados", "Riqueza de espécies") + subt + tema_gg(e) +
        ggplot2::theme(legend.key.width = grid::unit(2.2, "lines"))
      return(gg(p))
    }
    if (kk == "familias" && !is.null(X$familias)) {
      # nome da família inteiro numa linha só (letra menor e figura mais larga) e "Famílias" como nome do eixo X
      d <- X$familias; d$familia <- factor(d$familia, levels = unique(d$familia))
      d$grupo <- factor(ifelse(d$grupo_pad == "Anuros", "Anfíbios", d$grupo_pad))
      cores <- setNames(c(escuro(pal[1]), claro(pal[1]), pal[3:8])[seq_len(nlevels(d$grupo))], levels(d$grupo))
      p <- ggplot2::ggplot(d, ggplot2::aes(familia, S, fill = grupo)) + ggplot2::geom_col(width = .62) + rotulo_val(ggplot2::aes(label = S)) +
        ggplot2::scale_fill_manual(values = cores) + y0 + tt(chave, "Riqueza por Família", "Famílias") + subt + tema_gg(e) + sem_y +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = ggplot2::rel(.72)), axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 14)))
      if (nlevels(d$grupo) < 2) p <- p + ggplot2::theme(legend.position = "none")
      return(gg(p, max(w, .95 * nrow(d) + 1), h))
    }
    if (kk == "abundantes" && !is.null(X$abundantes)) {
      d <- head(X$abundantes, 10); d$rot <- factor(abrev_quebra(d$especie), levels = abrev_quebra(d$especie))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, n)) + ggplot2::geom_col(fill = cor, width = .62) +
        rotulo_val(ggplot2::aes(label = sprintf("%d\n(%s%%)", as.integer(n), fmt(100 * relativa)))) + y0 +
        tt(chave, "Espécies Mais Abundantes") + subt + tema_gg(e) + sem_y + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic", size = ggplot2::rel(.72))), larg_especies(nrow(d), w), h))
    }
    if (kk == "pontos") { pp <- X$por_ponto; pp <- pp[pp$S > 0 | pp$N > 0, ]; if (!nrow(pp)) return(invisible())
      return(barras_sn(data.frame(estacao = s, categoria = pp$ponto, S = pp$S, N = pp$N), chave, "Riqueza e Abundância por Ponto") ) }
    if (kk == "fito" && !is.null(X$fito)) return(barras_sn(data.frame(estacao = s, categoria = X$fito$fitofisionomia, S = X$fito$S, N = X$fito$N), chave, "Riqueza e Abundância por Fitofisionomia"))
    if (kk == "metodo" && !is.null(X$metodo)) return(barras_sn(data.frame(estacao = s, categoria = X$metodo$metodo, S = X$metodo$S, N = X$metodo$N), chave, "Riqueza e Abundância por Método", quebra = TRUE))
    # ponto com H' = 0 (uma espécie só ou nenhum registro) não aparece (pedido do usuário, 24/11/2026)
    if (kk == "diversidade") { pp <- X$por_ponto[X$por_ponto$N > 0 & !is.na(X$por_ponto$H) & X$por_ponto$H > 1e-9, ]; if (nrow(pp) < 2) return(invisible())
      l <- rbind(data.frame(ponto = pp$ponto, medida = "Shannon (H')", v = pp$H), data.frame(ponto = pp$ponto, medida = "Pielou (J')", v = pp$J))
      l$medida <- factor(l$medida, levels = c("Shannon (H')", "Pielou (J')")); l$ponto <- factor(l$ponto, levels = pp$ponto)
      return(gg(ggplot2::ggplot(l, ggplot2::aes(ponto, v, fill = medida)) + ggplot2::geom_col(position = ggplot2::position_dodge(.82), width = .78, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(v), "", fmt(v, 2))), position = ggplot2::position_dodge(.82), vjust = -.5, family = fam, fontface = "bold", size = tam * .6, colour = k$tinta, na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = setNames(c(escuro(cor), claro(cor)), levels(l$medida))) + y0 + tt(chave, "Diversidade por Ponto") + subt + tema_gg(e) + sem_y +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", colour = k$tinta)))) }
    if (kk == "rarefacao_fito" && !is.null(X$fito_curvas)) {
      d <- X$fito_curvas; d$fitofisionomia <- factor(d$fitofisionomia, levels = X$fito$fitofisionomia); fim <- do.call(rbind, lapply(split(d, d$fitofisionomia), function(q) q[nrow(q), ]))
      cf <- setNames(c(pal[3], pal[6], pal[4], pal[7], pal[8], pal[5])[seq_len(nlevels(d$fitofisionomia))], levels(d$fitofisionomia))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(n, S, colour = fitofisionomia)) + ggplot2::geom_vline(xintercept = X$fito_nmin, linetype = 3, colour = k$tinta2) +
        ggplot2::geom_line(linewidth = e$linha * 1.2) + ggplot2::geom_point(data = fim, size = e$ponto * 1.1) +
        ggplot2::scale_colour_manual(values = cf, labels = function(x) embrulha(x, 34)) + ggplot2::scale_y_continuous(limits = c(0, NA)) +
        tt(chave, "Rarefação por Fitofisionomia", "Número de indivíduos", "Riqueza de espécies", sprintf("Linha pontilhada: rarefação para n = %d indivíduos.", as.integer(X$fito_nmin))) + subt + tema_gg(e) + leg_fito(), w, h * 1.1)) }
    if (kk == "ranking" && !is.null(X$abundantes)) {
      d <- X$abundantes; d$posicao <- seq_len(nrow(d)); top <- head(d, 4); top$ord <- seq_len(nrow(top)); xr <- max(d$posicao)
      top$yl <- max(d$relativa) * .78^(top$ord - 1); top$xl <- top$posicao + xr * .12   # rótulos em escada: não se sobrepõem
      return(gg(ggplot2::ggplot(d, ggplot2::aes(posicao, relativa)) + ggplot2::geom_line(colour = cor, linewidth = e$linha) + ggplot2::geom_point(colour = cor, size = e$ponto * .8) +
        ggplot2::geom_segment(data = top, ggplot2::aes(xend = xl - xr * .005, yend = yl), colour = "#bdbdbd", linewidth = .3) +
        ggplot2::geom_text(data = top, ggplot2::aes(x = xl, y = yl, label = abrev_camp(especie)), hjust = 0, family = fam, fontface = "italic", size = tam * .8, colour = k$tinta2) +
        ggplot2::scale_y_log10(labels = function(x) paste0(formatC(100 * x, format = "fg", digits = 2, decimal.mark = ","), "%")) +
        tt(chave, "Curva de Ranking de Abundância", "Posição no ranking", "Abundância relativa (escala log)") + subt + tema_gg(e))) }
    if (kk == "dajoz" && !is.null(X$dajoz)) {
      lv <- c("Constante", "Acessória", "Acidental"); d <- data.frame(cat = factor(lv, levels = lv), n = as.numeric(table(factor(X$dajoz$categoria, levels = lv))))
      d$rot <- factor(c("Constante\n(50% ou mais)", "Acessória\n(25 a 50%)", "Acidental\n(< 25% dos pontos)"), levels = c("Constante\n(50% ou mais)", "Acessória\n(25 a 50%)", "Acidental\n(< 25% dos pontos)"))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, n)) + ggplot2::geom_col(fill = cor, width = .5) + rotulo_val(ggplot2::aes(label = n)) + y0 +
        tt(chave, "Constância de Ocorrência (Dajoz)") + subt + tema_gg(e) + sem_y)) }
    if (kk == "nmds" && !is.null(X$nmds)) {
      d <- X$nmds$pontos; d$fito[is.na(d$fito)] <- "Sem fitofisionomia"; fl <- unique(d$fito)
      cf <- setNames(c(pal[3], pal[6], pal[4], pal[7], pal[8], pal[5])[seq_along(fl)], fl)
      p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = fito))
      if (all(table(d$fito) >= 3)) p <- p + ggplot2::stat_ellipse(linewidth = .6, show.legend = FALSE)
      nota <- sprintf("Stress = %s%s", fmt(X$nmds$stress, 2), if (!is.null(X$permanova)) sprintf(" · PERMANOVA (fitofisionomia): R² = %s, p = %s", fmt(X$permanova$R2, 2), fmt(X$permanova$p, 3)) else "")
      return(gg(p + ggplot2::geom_point(size = e$ponto * 1.1) + ggplot2::geom_text(ggplot2::aes(label = ponto), vjust = 1.8, family = fam, size = tam * .7, colour = k$tinta2, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = cf, labels = function(x) embrulha(x, 34)) + ggplot2::coord_equal() + tt(chave, "Ordenação NMDS dos Pontos", "NMDS 1", "NMDS 2", nota) + subt + tema_gg(e) + leg_fito(), w, h * 1.1)) }
    if (kk == "upgma" && !is.null(X$upgma)) return(dendro_est(X$upgma$hc, X$upgma$fito, NULL, "Agrupamento dos Pontos (UPGMA)", chave, subt, e, gg, pal, k, w, h, fam, tam,
                                                            sprintf("Jaccard · correlação cofenética = %s", fmt(X$upgma$cofenetica, 2))))
    return(invisible())
  }
  kk <- sub("^estc_", "", chave); subt <- sub_(C$nome)
  if (kk == "pontos") return(barras_sn(C$pontos, chave, "Riqueza e Abundância por Ponto", ww = w * 1.15))
  if (kk == "grupos") return(barras_sn(C$grupos, chave, "Riqueza e Abundância por Estação"))
  if (kk == "fito" && !is.null(C$fito)) return(barras_sn(C$fito, chave, "Riqueza e Abundância por Fitofisionomia"))
  if (kk == "metodo" && !is.null(C$metodo)) return(barras_sn(C$metodo, chave, "Riqueza e Abundância por Método", quebra = TRUE))
  if (kk == "acumulacao") {
    # observada, Jackknife 1 e Bootstrap das duas campanhas juntas (pedido do usuário, 24/11/2026)
    d <- do.call(rbind, lapply(rot, function(s) { X <- C$campanhas[[s]]; q <- X$curva; if (is.null(q)) return(NULL)
      ce <- X$curva_est; q$jack1 <- if (!is.null(ce)) ce$valor[ce$estimador == "Jackknife 1"][match(q$pontos, ce$pontos[ce$estimador == "Jackknife 1"])] else NA_real_
      q$boot <- if (!is.null(ce)) ce$valor[ce$estimador == "Bootstrap"][match(q$pontos, ce$pontos[ce$estimador == "Bootstrap"])] else NA_real_
      cbind(campanha = s, q) })); if (is.null(d)) return(invisible())
    d$campanha <- factor(d$campanha, levels = rot)
    if (exists("curva_estimadores_gg")) return(curva_estimadores_gg(d, cs, e, k, gg, w, h, tt(chave, "Curva de Acumulação de Espécies", "Pontos acumulados", "Riqueza de espécies")))
    d$estacao <- d$campanha; fim <- do.call(rbind, lapply(split(d, d$estacao), function(q) q[nrow(q), ]))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(pontos, riqueza, colour = estacao, fill = estacao)) + ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), alpha = .15, colour = NA) +
      ggplot2::geom_line(linewidth = e$linha * 1.3) + ggplot2::geom_text(data = fim, ggplot2::aes(label = round(riqueza)), hjust = -.4, family = fam, fontface = "bold", size = tam * .9, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = cs) + ggplot2::scale_fill_manual(values = cs) + ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .08))) +
      ggplot2::scale_y_continuous(limits = c(0, NA)) + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 1.2))) +
      tt(chave, "Curva de Acumulação de Espécies", "Pontos acumulados", "Riqueza de espécies") + subt + tema_gg(e)))
  }
  if (kk == "estimadores" && !is.null(C$estimadores)) {
    d <- C$estimadores; d$estimador <- factor(d$estimador, levels = unique(d$estimador)); d$estacao <- factor(d$estacao, levels = rot)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(estimador, valor, fill = estacao)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .72) +
      ggplot2::geom_text(ggplot2::aes(label = fmt(valor)), position = ggplot2::position_dodge(.8), vjust = -.5, family = fam, fontface = "bold", size = tam * .78, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cs) + y0 + tt(chave, "Riqueza Observada e Estimada") + subt + tema_gg(e) + sem_y))
  }
  if (kk == "rarefacao" && !is.null(C$rarefacao_curvas)) {
    d <- C$rarefacao_curvas; d$estacao <- factor(d$estacao, levels = rot); gs <- unique(d$grupo); d$grupo <- factor(d$grupo, levels = gs)
    obs <- d[d$metodo == "Observed", ]; int <- d[d$metodo != "Extrapolation", ]; ext <- d[d$metodo != "Rarefaction", ]
    return(gg(ggplot2::ggplot(d, ggplot2::aes(m, qD, colour = estacao, fill = estacao)) + ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), alpha = .15, colour = NA) +
      ggplot2::geom_line(data = int, linewidth = e$linha) + ggplot2::geom_line(data = ext, linewidth = e$linha, linetype = 2) + ggplot2::geom_point(data = obs, size = e$ponto, shape = 21, colour = "white", stroke = .8) +
      ggplot2::facet_wrap(~grupo, scales = "free", nrow = 1) + ggplot2::scale_colour_manual(values = cs) + ggplot2::scale_fill_manual(values = cs) + ggplot2::scale_y_continuous(limits = c(0, NA)) +
      tt(chave, "Curvas de Rarefação e Extrapolação", "Número de indivíduos", "Riqueza de espécies", "Linha contínua: rarefação · tracejada: extrapolação até o dobro da amostra · ponto: amostra observada · faixa: IC 95%") +
      subt + tema_gg(e), w * max(1, length(gs) * .45), h))
  }
  if (kk %in% c("indices", "hill")) {
    R <- C$resumo
    d <- if (kk == "indices") rbind(data.frame(med = "Shannon (H')", estacao = R$estacao, v = R$shannon), data.frame(med = "Simpson (1 – D)", estacao = R$estacao, v = R$simpson),
                                    data.frame(med = "Equitabilidade\nde Pielou (J')", estacao = R$estacao, v = R$pielou), data.frame(med = "Dominância de\nBerger-Parker", estacao = R$estacao, v = R$berger_parker))
         else rbind(data.frame(med = "q = 0\nRiqueza", estacao = R$estacao, v = R$riqueza), data.frame(med = "q = 1\nShannon", estacao = R$estacao, v = R$hill1), data.frame(med = "q = 2\nSimpson", estacao = R$estacao, v = R$hill2))
    d$med <- factor(d$med, levels = unique(d$med)); d$estacao <- factor(d$estacao, levels = rot)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(med, v, fill = estacao)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .72) +
      ggplot2::geom_text(ggplot2::aes(label = fmt(v, if (kk == "indices") 2 else 1)), position = ggplot2::position_dodge(.8), vjust = -.5, family = fam, fontface = "bold", size = tam * .8, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cs) + y0 + tt(chave, if (kk == "indices") "Índices de Diversidade" else "Números de Hill (Espécies Efetivas)") + subt + tema_gg(e) + sem_y))
  }
  if (kk == "perfil") {
    d <- C$perfil; d$estacao <- factor(d$estacao, levels = rot); pt <- d[d$q %in% c(0, 1, 2), ]
    return(gg(ggplot2::ggplot(d, ggplot2::aes(q, D, colour = estacao)) + ggplot2::geom_line(linewidth = e$linha * 1.2) + ggplot2::geom_point(data = pt, size = e$ponto) +
      ggplot2::scale_colour_manual(values = cs) + tt(chave, "Perfil de Diversidade", "Ordem q (0 = riqueza · 1 = Shannon · 2 = Simpson)", "Espécies efetivas") + subt + tema_gg(e)))
  }
  if (kk == "ranking" && !is.null(C$ranking)) {
    d <- C$ranking; d$estacao <- factor(d$estacao, levels = rot)
    top <- do.call(rbind, lapply(split(d, d$estacao), function(q) head(q, 3))); top$ord <- seq_len(nrow(top))
    xr <- max(d$posicao)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(posicao, relativa, colour = estacao)) + ggplot2::geom_line(linewidth = e$linha) + ggplot2::geom_point(size = e$ponto * .8) +
      ggplot2::geom_segment(data = top, ggplot2::aes(xend = posicao + xr * .08, yend = max(d$relativa) * .82^(ord - 1)), linewidth = .3, show.legend = FALSE) +
      ggplot2::geom_text(data = top, ggplot2::aes(x = posicao + xr * .085, y = max(d$relativa) * .82^(ord - 1), label = abrev_camp(especie)), hjust = 0, family = fam, fontface = "italic", size = tam * .78, show.legend = FALSE) +
      ggplot2::scale_y_log10(labels = function(x) paste0(formatC(100 * x, format = "fg", digits = 2, decimal.mark = ","), "%")) + ggplot2::scale_colour_manual(values = cs) +
      tt(chave, "Curva de Ranking de Abundância", "Posição no ranking", "Abundância relativa (escala log)") + subt + tema_gg(e)))
  }
  if (kk == "dajoz" && !is.null(C$dajoz)) {
    lv <- c("Constante", "Acessória", "Acidental"); rl <- c("Constante\n(50% ou mais)", "Acessória\n(25 a 50%)", "Acidental\n(< 25% dos pontos)")
    d <- do.call(rbind, lapply(rot, function(s) data.frame(estacao = s, cat = factor(rl, levels = rl), n = as.numeric(table(factor(C$dajoz$categoria[C$dajoz$estacao == s], levels = lv))))))
    d$estacao <- factor(d$estacao, levels = rot)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(cat, n, fill = estacao)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .72) +
      ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_dodge(.8), vjust = -.5, family = fam, fontface = "bold", size = tam * .85, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cs) + y0 + tt(chave, "Constância de Ocorrência (Dajoz)") + subt + tema_gg(e) + sem_y))
  }
  if (kk == "pareada" && !is.null(C$pareada_pontos) && nrow(C$pareada_pontos) >= 2) {
    P <- C$pareada_pontos; a <- rot[1]; b <- rot[2]
    d <- rbind(data.frame(medida = "Riqueza", ponto = P$ponto, estacao = a, v = P$S_a), data.frame(medida = "Riqueza", ponto = P$ponto, estacao = b, v = P$S_b),
               data.frame(medida = "Abundância", ponto = P$ponto, estacao = a, v = P$N_a), data.frame(medida = "Abundância", ponto = P$ponto, estacao = b, v = P$N_b))
    d$estacao <- factor(d$estacao, levels = c(a, b)); d$medida <- factor(d$medida, levels = c("Riqueza", "Abundância"))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(estacao, v)) + ggplot2::geom_boxplot(ggplot2::aes(fill = estacao), width = .38, outlier.shape = NA, colour = k$tinta2, alpha = .25, show.legend = FALSE) +
      ggplot2::geom_line(ggplot2::aes(group = ponto), colour = "#cfcfcf", linewidth = .4) + ggplot2::geom_point(ggplot2::aes(colour = estacao), size = e$ponto * .9, show.legend = FALSE) +
      ggplot2::facet_wrap(~medida, scales = "free_y") + ggplot2::scale_fill_manual(values = cs) + ggplot2::scale_colour_manual(values = cs) +
      tt(chave, "Riqueza e Abundância por Ponto", NULL, NULL, if (!is.null(C$pareada)) paste(sprintf("%s: GLMM p = %s · Wilcoxon pareado p = %s", C$pareada$variavel, fmt(C$pareada$p_glmm, 3), fmt(C$pareada$p_wilcoxon, 3)), collapse = "   ") else NULL) +
      subt + tema_gg(e) + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", colour = k$tinta, size = ggplot2::rel(1)))))
  }
  if (kk == "nmds" && !is.null(C$nmds)) {
    d <- C$nmds$pontos; d$estacao <- factor(d$estacao, levels = rot); d$fito[is.na(d$fito)] <- "Sem fitofisionomia"
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y))
    if (all(table(d$estacao) >= 3)) p <- p + ggplot2::stat_ellipse(ggplot2::aes(colour = estacao, fill = estacao), geom = "polygon", alpha = .08, linewidth = .6, show.legend = FALSE)
    pv <- C$permanova; nota <- sprintf("Stress = %s%s%s", fmt(C$nmds$stress, 2), if (C$nmds$stress < .05) " (muito baixo: ordenação possivelmente degenerada, leia com cuidado)" else "", if (!is.null(pv) && any(pv$teste == "PERMANOVA · estação")) sprintf(" · PERMANOVA (estação): R² = %s, p = %s", fmt(pv$R2[pv$teste == "PERMANOVA · estação"], 2), fmt(pv$p[pv$teste == "PERMANOVA · estação"], 3)) else "")
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = estacao, shape = fito), size = e$ponto * 1.1) + ggplot2::scale_colour_manual(values = cs) + ggplot2::scale_fill_manual(values = cs) +
      ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 3, 8)[seq_along(unique(d$fito))], labels = function(x) embrulha(x, 34)) +
      ggplot2::guides(colour = ggplot2::guide_legend(order = 1, override.aes = list(size = 3)), shape = ggplot2::guide_legend(order = 2, ncol = 2, byrow = TRUE, override.aes = list(colour = k$tinta2))) +
      ggplot2::coord_equal() + tt(chave, "Ordenação NMDS (Bray-Curtis)", "NMDS 1", "NMDS 2", nota) + subt + tema_gg(e) +
      ggplot2::theme(legend.box = "vertical", legend.text = ggplot2::element_text(size = ggplot2::rel(.66), lineheight = .9), legend.key.height = grid::unit(.95, "lines"))
    if (length(unique(d$fito)) < 2) p <- p + ggplot2::guides(shape = "none")
    return(gg(p, w, h * 1.15))
  }
  if (kk == "simper" && !is.null(C$simper)) {
    d <- C$simper; d$rot <- factor(abrev_quebra(d$especie), levels = abrev_quebra(d$especie)); d$mais_em <- factor(d$mais_em, levels = rot)   # legenda só "Chuva" e "Seca" (pedido do usuário)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(rot, contribuicao, fill = mais_em)) + ggplot2::geom_col(width = .58) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(fmt(100 * contribuicao), "%")), vjust = -.5, family = fam, fontface = "bold", size = tam * .8, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cs, drop = FALSE) + y0 +
      tt(chave, "Espécies que Mais Diferenciam as Estações (SIMPER)") + subt + tema_gg(e) + sem_y + ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic", size = ggplot2::rel(.72))), larg_especies(nrow(d), w), h))
  }
  if (kk == "baselga" && !is.null(C$baselga) && nrow(C$baselga)) {
    B <- C$baselga; d <- rbind(data.frame(escala = B$escala, parte = "Substituição", v = B$substituicao), data.frame(escala = B$escala, parte = "Aninhamento", v = B$aninhamento))
    d$parte <- factor(d$parte, levels = c("Aninhamento", "Substituição")); d$escala <- factor(d$escala, levels = B$escala)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(escala, v, fill = parte)) + ggplot2::geom_col(width = .45) +
      ggplot2::geom_text(ggplot2::aes(label = fmt(v, 2)), position = ggplot2::position_stack(vjust = .5), family = fam, fontface = "bold", size = tam * .85, colour = "white") +
      ggplot2::geom_text(data = B, ggplot2::aes(x = escala, y = sorensen, label = paste0("Sørensen = ", fmt(sorensen, 2))), inherit.aes = FALSE, vjust = -.6, family = fam, fontface = "bold", size = tam * .85, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = c("Substituição" = pal[4], "Aninhamento" = claro(pal[4])), breaks = c("Substituição", "Aninhamento")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) + tt(chave, "Diversidade Beta entre Estações") + subt + tema_gg(e) + sem_y +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = ggplot2::rel(1)))))
  }
  if (kk == "upgma" && !is.null(C$upgma)) return(dendro_est(C$upgma$hc, C$upgma$estacao, cs, "Agrupamento das Unidades (UPGMA)", chave, subt, e, gg, pal, k, w * 1.1, h, fam, tam,
                                                         sprintf("Jaccard · correlação cofenética = %s", fmt(C$upgma$cofenetica, 2)), rotulos = C$upgma$ponto))
  if (kk == "indval" && !is.null(C$indval) && nrow(C$indval)) {
    d <- C$indval; d$rot <- factor(abrev_quebra(d$especie), levels = unique(abrev_quebra(d$especie))); d$grupo <- factor(d$grupo, levels = rot)
    p <- ggplot2::ggplot(d, ggplot2::aes(rot, 100 * indval, fill = grupo)) + ggplot2::geom_col(width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = fmt(100 * indval)), vjust = -.5, family = fam, fontface = "bold", size = tam * .7, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cs) + y0 + tt(chave, "Espécies Indicadoras das Estações (IndVal)", NULL, NULL, "IndVal com 999 permutações dentro do ponto; só espécies com p < 0,05.") + subt + tema_gg(e) + sem_y +
      ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic", size = ggplot2::rel(.72)))
    if (length(unique(d$grupo)) < 2) p <- p + ggplot2::theme(legend.position = "none") + ggplot2::labs(title = if (isTRUE(e$mostrar_titulo)) paste0("Espécies Indicadoras da ", ifelse(Ee$tipo[match(d$grupo[1], rot)] == "chuva", "Estação Chuvosa", ifelse(Ee$tipo[match(d$grupo[1], rot)] == "seca", "Estação Seca", as.character(d$grupo[1]))), " (IndVal)") else NULL)
    return(gg(p, larg_especies(nrow(d), w), h))
  }
  invisible()
}

# dendrograma em ggplot com a folha colorida (fitofisionomia ou estação)
dendro_est <- function(hc, cor_folha, cores, titulo, chave, subt, e, gg, pal, k, w, h, fam, tam, nota, rotulos = NULL) {
  d <- list(rotulos = hc$labels, ordem = hc$order, merge = hc$merge, altura = hc$height)
  sg <- dendro_segmentos(d)
  fo <- sg$folhas; fo$grupo <- unname(cor_folha[fo$rotulo]); fo$grupo[is.na(fo$grupo)] <- "Sem fitofisionomia"
  if (!is.null(rotulos)) fo$rotulo <- unname(rotulos[fo$rotulo])
  gl <- unique(fo$grupo); if (is.null(cores)) cores <- setNames(c(pal[3], pal[6], pal[4], pal[7], pal[8], pal[5])[seq_along(gl)], gl)
  gg(ggplot2::ggplot() + ggplot2::geom_segment(data = sg$seg, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), colour = "#9a9a9a", linewidth = .45) +
    ggplot2::geom_point(data = fo, ggplot2::aes(x = x, y = 0, colour = grupo), size = e$ponto * .9) +
    ggplot2::geom_text(data = fo, ggplot2::aes(x = x, y = -max(sg$seg$y) * .04, label = rotulo), angle = 90, hjust = 1, family = fam, size = tam * .7, colour = k$tinta2) +
    ggplot2::scale_colour_manual(values = cores, labels = function(x) embrulha(x, 34)) + ggplot2::scale_x_continuous(NULL, breaks = NULL, expand = ggplot2::expansion(add = .6)) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
    ggplot2::scale_y_continuous(limits = c(-max(sg$seg$y) * .16, NA), breaks = function(l) pretty(c(0, l[2]))[pretty(c(0, l[2])) >= 0]) +
    titulos_gg(e, chave, titulo, NULL, "Dissimilaridade de Jaccard", nota) + subt + tema_gg(e) +
    ggplot2::theme(axis.line.x = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = ggplot2::rel(.66), lineheight = .9), legend.key.height = grid::unit(.95, "lines")), w, h)
}
