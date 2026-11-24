# Quadrante: várias campanhas.
# Desde 23/11/2026 (noite, pedido do usuário) a unidade amostral continua sendo o PONTO, com as campanhas somadas:
# descritiva, índices, curvas, similaridade, dendrograma e ordenação usam P1, P2... com todos os registros de chuva e seca.
# A comparação entre campanhas (estações) fica só na seção Campanhas: cada campanha como um todo
# (riqueza, famílias, exclusivas, curva, estimadores e índices) e, por ponto, riqueza e abundância em cada campanha.
# O código da campanha é o próprio nome (antes era C1, C2...).

codigo_campanhas <- function(camp) { u <- sort(unique(camp)); setNames(u, u) }

unidades_campanha <- function(L) {
  r <- L$registros
  P <- L$pontos
  camp_pontos <- P$campanha
  cod <- codigo_campanhas(c(r$campanha, camp_pontos[!is.na(camp_pontos) & nzchar(camp_pontos)]))
  r$ponto_campo <- r$ponto; r$camp_cod <- unname(cod[r$campanha])
  L$registros <- r
  # visitas sem registro informadas na aba de pontos (coluna Campanha), para a comparação por ponto
  visitas <- if (!is.null(P) && "campanha" %in% names(P)) { v <- P[!is.na(P$campanha) & nzchar(P$campanha), c("ponto", "campanha"), drop = FALSE]; unique(v) } else NULL
  if (!is.null(P) && anyDuplicated(P$ponto)) {
    Q <- P[!duplicated(P$ponto), , drop = FALSE]
    for (pt in unique(P$ponto[duplicated(P$ponto)])) {
      co <- unique(na.omit(P$coordenadas[P$ponto == pt]))
      Q$coordenadas[Q$ponto == pt] <- if (length(co) == 1) co else NA_character_
      es <- suppressWarnings(as.numeric(P$esforco[P$ponto == pt]))
      if ("esforco" %in% names(Q)) Q$esforco[Q$ponto == pt] <- if (any(is.finite(es))) sum(es, na.rm = TRUE) else NA_real_
    }
    P <- Q
  }
  if (!is.null(P)) P$campanha <- NULL
  L$pontos <- P; L$pontos_campo <- P
  L$campanhas <- data.frame(codigo = unname(cod), campanha = names(cod), stringsAsFactors = FALSE)
  L$visitas_campanha <- visitas
  L
}

comparar_campanhas <- function(reg, excluidas, campanhas, pontos_info = NULL) {
  visitas <- unique(reg[c("ponto_campo", "camp_cod")])
  vc <- attr(campanhas, "visitas")
  if (!is.null(vc) && nrow(vc)) {
    cadastro <- data.frame(ponto_campo = vc$ponto, camp_cod = campanhas$codigo[match(vc$campanha, campanhas$campanha)])
    visitas <- unique(rbind(visitas, cadastro[!is.na(cadastro$camp_cod), , drop = FALSE]))
  }
  r <- reg[!(reg$especie %in% excluidas) & !is.na(reg$total) & reg$total > 0, ]
  cods <- campanhas$codigo[campanhas$codigo %in% visitas$camp_cod]
  if (length(cods) < 2 || !nrow(r)) return(NULL)
  mc <- tapply(r$total, list(factor(r$camp_cod, levels = cods), r$especie), sum); mc[is.na(mc)] <- 0
  pres <- mc > 0
  r$familia[is.na(r$familia) | !nzchar(r$familia)] <- "Família não informada"
  # cada campanha (estação) como um todo: os pontos visitados nela são as unidades da curva e dos estimadores
  por_camp <- lapply(setNames(cods, cods), function(c) {
    rc <- r[r$camp_cod == c, , drop = FALSE]
    pts <- sort(unique(c(visitas$ponto_campo[visitas$camp_cod == c], rc$ponto_campo)))
    ab <- tapply(rc$total, list(factor(rc$ponto_campo, levels = pts), rc$especie), sum); ab[is.na(ab)] <- 0; ab <- as.matrix(ab)
    pa <- (ab > 0) * 1
    curva <- est <- NULL
    if (length(pts) >= 2 && ncol(pa) >= 1) {
      sa <- tryCatch(vegan::specaccum(pa, method = "random", permutations = 199), error = function(e) NULL)
      if (!is.null(sa)) curva <- data.frame(campanha = c, pontos = sa$sites, riqueza = round(sa$richness, 3), dp = round(sa$sd, 3),
        li = round(pmax(0, sa$richness - 1.96 * sa$sd), 3), ls = round(sa$richness + 1.96 * sa$sd, 3), stringsAsFactors = FALSE)
      # Jackknife 1 e Bootstrap ponto a ponto (poolaccum), para a curva mostrar observada e estimadores juntos (pedido do usuário, 24/11/2026)
      if (!is.null(curva)) { curva$jack1 <- NA_real_; curva$boot <- NA_real_ }
      if (!is.null(curva) && length(pts) >= 3) {
        pl <- tryCatch(summary(vegan::poolaccum(pa, permutations = 199, minsize = 2), display = c("jack1", "boot")), error = function(e) NULL)
        if (!is.null(pl)) { curva$jack1 <- round(pl$jack1[match(curva$pontos, pl$jack1[, 1]), 2], 3); curva$boot <- round(pl$boot[match(curva$pontos, pl$boot[, 1]), 2], 3) }
      }
      sp <- tryCatch(vegan::specpool(pa), error = function(e) NULL)
      if (!is.null(sp)) est <- c(chao2 = round(sp$chao, 1), jack1 = round(sp$jack1, 1), jack2 = round(sp$jack2, 1), bootstrap = round(sp$boot, 1),
                                 chao2_ep = round(sp$chao.se, 2), jack1_ep = round(sp$jack1.se, 2), bootstrap_ep = round(sp$boot.se, 2))
    }
    ia <- indices_area(ab)
    v <- sort(colSums(ab), decreasing = TRUE); v <- v[v > 0]
    dom <- if (length(v)) data.frame(campanha = c, posicao = seq_along(v), especie = names(v), individuos = as.numeric(v),
      relativa = round(as.numeric(v) / sum(v), 4), pontos = as.integer(colSums(pa[, names(v), drop = FALSE])), stringsAsFactors = FALSE) else NULL
    list(pontos = length(pts), familias = length(unique(rc$familia)), curva = curva, est = est, indices = ia, dominancia = dom)
  })
  iv <- function(c, medida) por_camp[[c]]$indices$valor[por_camp[[c]]$indices$medida == medida]
  ev <- function(c, k) if (is.null(por_camp[[c]]$est)) NA_real_ else unname(por_camp[[c]]$est[k])
  resumo <- data.frame(codigo = cods, campanha = campanhas$campanha[match(cods, campanhas$codigo)],
    riqueza = rowSums(pres), individuos = rowSums(mc),
    familias = vapply(cods, function(c) por_camp[[c]]$familias, 1L),
    pontos_amostrados = vapply(cods, function(c) por_camp[[c]]$pontos, 1L),
    pontos_com_registro = vapply(cods, function(c) length(unique(r$ponto_campo[r$camp_cod == c])), 1L),
    exclusivas = vapply(seq_along(cods), function(i) sum(pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0), 1L),
    shannon = vapply(cods, iv, 1, "Shannon (H')"), simpson = vapply(cods, iv, 1, "Simpson (1 - D)"),
    pielou = vapply(cods, iv, 1, "Equitabilidade de Pielou (J')"), berger_parker = vapply(cods, iv, 1, "Berger-Parker (dominância)"),
    chao2 = vapply(cods, ev, 1, "chao2"), jackknife1 = vapply(cods, ev, 1, "jack1"),
    stringsAsFactors = FALSE, row.names = NULL)
  resumo$completude <- round(resumo$riqueza / resumo$jackknife1, 3)
  resumo$dominante <- vapply(cods, function(c) { d <- por_camp[[c]]$dominancia; if (is.null(d)) NA_character_ else d$especie[1] }, "")
  # estimadores de riqueza de cada campanha (pontos da campanha como unidades)
  nomes_est <- c(Observada = "obs", `Chao 2` = "chao2", `Jackknife 1` = "jack1", `Jackknife 2` = "jack2", Bootstrap = "bootstrap")
  estimadores <- do.call(rbind, lapply(cods, function(c) { E <- por_camp[[c]]$est; S <- resumo$riqueza[resumo$codigo == c]
    val <- vapply(nomes_est, function(k) if (k == "obs") S else if (is.null(E)) NA_real_ else unname(E[k]), 1)
    ep <- vapply(nomes_est, function(k) if (is.null(E) || !paste0(k, "_ep") %in% names(E)) NA_real_ else unname(E[paste0(k, "_ep")]), 1)
    data.frame(campanha = c, estimador = names(nomes_est), valor = val, ep = ep, completude = round(S / val, 3), stringsAsFactors = FALSE, row.names = NULL) }))
  dominancia <- do.call(rbind, lapply(por_camp, `[[`, "dominancia")); rownames(dominancia) <- NULL
  indices <- data.frame(medida = por_camp[[1]]$indices$medida, stringsAsFactors = FALSE)
  for (c in cods) indices[[c]] <- por_camp[[c]]$indices$valor
  curvas <- do.call(rbind, lapply(por_camp, `[[`, "curva")); rownames(curvas) <- NULL
  fam <- r$familia[match(colnames(mc), r$especie)]
  esp <- data.frame(especie = colnames(mc), familia = fam, t(mc), check.names = FALSE, stringsAsFactors = FALSE)
  em <- function(v) if (all(v)) "todas" else paste(cods[v], collapse = " e ")
  esp$em <- apply(pres, 2, em)
  esp <- esp[order(esp$familia, esp$especie), ]; rownames(esp) <- NULL
  # famílias: espécies e indivíduos de cada família em cada campanha
  fl <- sort(unique(fam))
  familias <- data.frame(familia = fl, stringsAsFactors = FALSE)
  for (c in cods) {
    familias[[paste0("S_", c)]] <- vapply(fl, function(f) sum(pres[c, fam == f]), 1L)
    familias[[paste0("N_", c)]] <- vapply(fl, function(f) sum(mc[c, fam == f]), 1)
  }
  pf <- do.call(cbind, lapply(cods, function(c) familias[[paste0("S_", c)]] > 0)); familias$em <- apply(pf, 1, em)
  exclusivas <- esp[esp$em != "todas" & !grepl(" e ", esp$em), c("especie", "familia", "em"), drop = FALSE]
  if (nrow(exclusivas)) { exclusivas$individuos <- vapply(seq_len(nrow(exclusivas)), function(i) mc[exclusivas$em[i], exclusivas$especie[i]], 1)
    names(exclusivas)[3] <- "campanha"; exclusivas <- exclusivas[order(match(exclusivas$campanha, cods), exclusivas$familia, exclusivas$especie), ]; rownames(exclusivas) <- NULL }
  # Shannon de cada campanha comparado pelo teste t de Hutcheson (1970), como nos inventários com chuva e seca
  hut <- tryCatch({ h <- hutcheson(mc); names(h$pares)[1:4] <- c("campanha_a", "campanha_b", "H_a", "H_b"); names(h$ic)[1] <- "campanha"; h }, error = function(e) NULL)
  # rarefação e extrapolação por indivíduos (iNEXT, q = 0) de cada campanha, até o dobro da maior amostra
  rar <- tryCatch({ vs <- lapply(cods, function(c) sort(mc[c, mc[c, ] > 0], decreasing = TRUE)); names(vs) <- cods
    vs <- vs[vapply(vs, function(v) length(v) >= 2 && sum(v) >= 4, TRUE)]
    if (length(vs) < 1) NULL else { o <- suppressWarnings(iNEXT::iNEXT(vs, q = 0, datatype = "abundance", endpoint = 2 * max(vapply(vs, sum, 1)), knots = 40, nboot = 100))
      z <- o$iNextEst$size_based; data.frame(campanha = as.character(z$Assemblage), m = z$m, metodo = as.character(z$Method), qD = round(z$qD, 3), li = round(z$qD.LCL, 3), ls = round(z$qD.UCL, 3), cobertura = round(z$SC, 4), stringsAsFactors = FALSE) } },
    error = function(e) NULL)
  # espécies mais abundantes no total, com os indivíduos de cada campanha
  tot <- colSums(mc); o <- order(-tot)
  mais_abundantes <- data.frame(posicao = seq_along(o), especie = colnames(mc)[o], familia = fam[o], t(mc)[o, , drop = FALSE], total = as.numeric(tot[o]),
    relativa = round(as.numeric(tot[o]) / sum(tot), 4), em = apply(pres, 2, em)[o], check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  # compartilhadas: registradas em todas as campanhas, com os indivíduos de cada uma
  compartilhadas <- esp[esp$em == "todas", c("especie", "familia", cods), drop = FALSE]; rownames(compartilhadas) <- NULL
  pares <- do.call(rbind, combn(cods, 2, simplify = FALSE, FUN = function(p) {
    a <- pres[p[1], ]; b <- pres[p[2], ]; com <- sum(a & b)
    data.frame(campanha_a = p[1], campanha_b = p[2], comuns = com, so_a = sum(a & !b), so_b = sum(b & !a),
      jaccard = if (any(a | b)) round(com / sum(a | b), 3) else NA_real_, sorensen = if (any(a | b)) round(2 * com / (sum(a) + sum(b)), 3) else NA_real_,
      bray_curtis_sim = if (sum(mc[p, , drop = FALSE]) > 0) round(1 - as.numeric(vegan::vegdist(mc[p, , drop = FALSE], "bray")), 3) else NA_real_, stringsAsFactors = FALSE) }))
  pts <- sort(unique(visitas$ponto_campo)); pts <- pts[order(suppressWarnings(as.integer(sub("\\D+", "", pts))), pts)]
  porponto <- do.call(rbind, lapply(pts, function(p) {
    s <- lapply(cods, function(c) unique(r$especie[r$ponto_campo == p & r$camp_cod == c]))
    linha <- data.frame(ponto = p, stringsAsFactors = FALSE)
    visitado <- cods %in% visitas$camp_cod[visitas$ponto_campo == p]
    for (i in seq_along(cods)) {
      linha[[paste0("S_", cods[i])]] <- if (visitado[i]) length(s[[i]]) else NA_integer_
      linha[[paste0("N_", cods[i])]] <- if (visitado[i]) sum(r$total[r$ponto_campo == p & r$camp_cod == cods[i]]) else NA_real_
    }
    u <- unique(unlist(s)); comum <- Reduce(intersect, s)
    linha$comuns <- if (all(visitado)) length(comum) else NA_integer_
    linha$jaccard <- if (all(visitado) && length(u)) round(length(comum) / length(u), 3) else NA_real_
    linha }))
  # só os pontos com algum registro em alguma campanha (ponto vazio nas duas não entra)
  sc <- porponto[, paste0("S_", cods), drop = FALSE]
  porponto <- porponto[rowSums(sc > 0, na.rm = TRUE) > 0, , drop = FALSE]; rownames(porponto) <- NULL
  num <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d, decimal.mark = ","), "-")
  um <- function(i) sprintf("%s: %d espécies de %d famílias, %d indivíduos, %d exclusivas; Shannon H' = %s, Pielou J' = %s; Jackknife 1 estima %s espécies (amostrado %s do esperado)",
    resumo$campanha[i], resumo$riqueza[i], resumo$familias[i], as.integer(resumo$individuos[i]), resumo$exclusivas[i], num(resumo$shannon[i]), num(resumo$pielou[i]),
    num(resumo$jackknife1[i], 1), if (is.finite(resumo$completude[i])) paste0(round(100 * resumo$completude[i]), "%") else "-")
  fam_so <- lapply(cods, function(c) familias$familia[familias$em == c])
  texto <- txt(
    "Cada campanha (ou estação) é analisada como um todo: as espécies e famílias registradas nela, as exclusivas (só nela), a curva de acumulação e os estimadores de riqueza usando os pontos visitados naquela campanha, e os índices de diversidade com todos os pontos da campanha somados. Para cada par, Jaccard e Sørensen medem a semelhança das listas e Bray-Curtis considera a abundância.",
    paste0(paste(vapply(seq_along(cods), um, ""), collapse = ". "), ".",
      if (any(lengths(fam_so) > 0)) paste0(" Famílias só numa campanha: ", paste(sprintf("%s (%s)", vapply(fam_so, function(f) if (length(f)) paste(f, collapse = ", ") else "nenhuma", ""), cods), collapse = "; "), ".") else " Todas as famílias aparecem em todas as campanhas."),
    "Compare as campanhas pelas curvas e pela completude, não só pela riqueza bruta: uma campanha com mais pontos ou mais indivíduos tende a ter mais espécies só por isso. Curva ainda subindo indica que faltam espécies naquela campanha. A tabela por ponto mostra onde a lista mudou de uma campanha para a outra.",
    "Diferença entre campanhas mistura estação, clima da noite, esforço e quem amostrou. Sem a data e o esforço de cada campanha, não dá para separar o efeito da estação do efeito do método. Com poucos pontos por campanha, os estimadores são instáveis.")
  if (!is.null(hut)) texto$resultado <- paste0(texto$resultado, " Teste t de Hutcheson entre as campanhas: ",
    paste(sprintf("%s H' = %s × %s H' = %s, t = %s, p = %s (%s)", hut$pares$campanha_a, num(hut$pares$H_a), hut$pares$campanha_b, num(hut$pares$H_b), num(hut$pares$t), num(hut$pares$p, 3), hut$pares$diferenca), collapse = "; "), ".")
  ex_txt <- vapply(cods, function(c) { e <- exclusivas$especie[exclusivas$campanha == c]; sprintf("só na %s, %d: %s", c, length(e), if (length(e)) paste(e, collapse = ", ") else "nenhuma") }, "")
  texto$resultado <- paste0(texto$resultado, " Espécies ", paste(ex_txt, collapse = "; "), ". Compartilhadas (", nrow(compartilhadas), "): ",
    if (nrow(compartilhadas)) paste(compartilhadas$especie, collapse = ", ") else "nenhuma", ".",
    paste0(" Espécie mais abundante: ", paste(sprintf("%s, %s (%s dos indivíduos)", cods, resumo$dominante,
      vapply(cods, function(c) { d <- por_camp[[c]]$dominancia; if (is.null(d)) "-" else paste0(round(100 * d$relativa[1]), "%") }, "")), collapse = "; "), "."))
  list(resumo = resumo, especies = esp, familias = familias, exclusivas = exclusivas, compartilhadas = compartilhadas,
       estimadores = estimadores, dominancia = dominancia, indices = indices, curvas = curvas,
       hutcheson = hut, rarefacao = rar, mais_abundantes = mais_abundantes,
       pares = pares, por_ponto = porponto, texto = texto)
}

# ---- figuras ----
catalogo_camp <- function() list(
  campanhas_curvas = list(nome = "Campanhas: curva de acumulação de cada uma", padrao = "curvas", formas = list(curvas = "Uma curva por campanha, com a faixa de 95%")),
  campanhas_familias = list(nome = "Campanhas: famílias", padrao = "familias", formas = list(familias = "Espécies de cada família em cada campanha")),
  campanhas_rarefacao = list(nome = "Campanhas: rarefação e extrapolação (iNEXT)", padrao = "rarefacao", formas = list(rarefacao = "Uma curva por campanha: cheia até a amostra, tracejada na extrapolação")),
  campanhas_estimadores = list(nome = "Campanhas: estimadores de riqueza", padrao = "estimadores", formas = list(estimadores = "Observada, Chao 2, Jackknife 1 e 2 e Bootstrap de cada campanha")),
  campanhas_dominancia = list(nome = "Campanhas: dominância", padrao = "dominancia", formas = list(dominancia = "Curva de dominância (ranque de abundância) de cada campanha")),
  campanhas = list(nome = "Campanhas: espécies em comum", padrao = "composicao", formas = list(composicao = "Espécies registradas em mais de uma campanha, com a abundância em cada")),
  campanhas_so_1 = list(nome = "Campanhas: espécies só na 1ª campanha", padrao = "exclusivas", formas = list(exclusivas = "Espécies só na primeira campanha, com a abundância")),
  campanhas_so_2 = list(nome = "Campanhas: espécies só na 2ª campanha", padrao = "exclusivas", formas = list(exclusivas = "Espécies só na segunda campanha, com a abundância")),
  campanhas_so_3 = list(nome = "Campanhas: espécies só na 3ª campanha", padrao = "exclusivas", formas = list(exclusivas = "Espécies só na terceira campanha, com a abundância")),
  campanhas_so_4 = list(nome = "Campanhas: espécies só na 4ª campanha", padrao = "exclusivas", formas = list(exclusivas = "Espécies só na quarta campanha, com a abundância")),
  campanhas_pontos = list(nome = "Campanhas: riqueza de cada ponto", padrao = "pontos", formas = list(pontos = "Pontos ligados entre campanhas")),
  campanhas_especies = list(nome = "Campanhas: espécies e abundância", padrao = "especies", formas = list(especies = "Círculos do tamanho da abundância")),
  nmds = list(nome = "Ordenação NMDS", padrao = "grupos", formas = list(grupos = "Pontos com o contorno de cada grupo", rotulos = "Só pontos com nome"))
)

# cor de cada campanha: chuva #00a5bf (pal[1]) e seca #ee7d00 (pal[2]) quando o nome diz a estação
cores_camp <- function(cods, pal) {
  t <- if (exists("tipo_estacao")) tipo_estacao(cods) else rep("outra", length(cods))
  setNames(ifelse(t == "chuva", pal[1], ifelse(t == "seca", pal[2], pal[(seq_along(cods) + 1) %% length(pal) + 1])), cods)
}
# afasta rótulos de valores parecidos: mantém a ordem e uma distância mínima (gap) entre eles
espalha_rotulos <- function(y, gap) {
  if (length(y) < 2 || !is.finite(gap) || gap <= 0) return(y)
  o <- order(y); z <- y[o]
  for (i in 2:length(z)) z[i] <- max(z[i], z[i - 1] + gap)   # só empurra para cima quem está colado no de baixo
  r <- y; r[o] <- z; r
}
# curva de acumulação com observada (faixa de 95%), Jackknife 1 e Bootstrap de cada campanha (pedido do usuário, 24/11/2026)
# d: campanha (fator), pontos, riqueza, li, ls, jack1, boot
curva_estimadores_gg <- function(d, cc, e, k, gg, w, h, rotulos) {
  med <- c("Observada", "Jackknife 1", "Bootstrap")
  l <- rbind(data.frame(campanha = d$campanha, pontos = d$pontos, medida = med[1], v = d$riqueza),
             if (!is.null(d$jack1)) data.frame(campanha = d$campanha, pontos = d$pontos, medida = med[2], v = d$jack1),
             if (!is.null(d$boot)) data.frame(campanha = d$campanha, pontos = d$pontos, medida = med[3], v = d$boot))
  l <- l[!is.na(l$v), ]; l$medida <- factor(l$medida, levels = intersect(med, unique(l$medida)))
  fim <- do.call(rbind, lapply(split(l, list(l$campanha, l$medida), drop = TRUE), function(q) q[which.max(q$pontos), ]))
  fim$yl <- espalha_rotulos(fim$v, diff(range(c(0, l$v, d$ls), na.rm = TRUE)) * .055)   # valores parecidos não se sobrepõem
  fmt <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
  gg(ggplot2::ggplot(l, ggplot2::aes(pontos, v, colour = campanha, linetype = medida, group = interaction(campanha, medida))) +
    ggplot2::geom_ribbon(data = d, ggplot2::aes(x = pontos, ymin = li, ymax = ls, fill = campanha), inherit.aes = FALSE, alpha = .1, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = e$linha * 1.15) + ggplot2::geom_point(data = fim, size = e$ponto * .9, show.legend = FALSE) +
    ggplot2::geom_text(data = fim, ggplot2::aes(y = yl, label = fmt(v)), hjust = -.3, family = e$familia, fontface = "bold", size = e$fonte / 3.6, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cc, name = NULL) + ggplot2::scale_fill_manual(values = cc, guide = "none") +
    ggplot2::scale_linetype_manual(values = c("Observada" = 1, "Jackknife 1" = 2, "Bootstrap" = 4), name = NULL) +
    ggplot2::scale_x_continuous(breaks = function(x) unique(round(pretty(x))), expand = ggplot2::expansion(mult = c(.02, .1))) + ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, override.aes = list(linetype = 1, linewidth = 1.2)),
                    linetype = ggplot2::guide_legend(order = 2, override.aes = list(colour = k$tinta2, linewidth = .8))) +
    rotulos + tema_gg(e) + ggplot2::theme(legend.key.width = grid::unit(2.2, "lines")), w, h * 1.05)
}

abrev_camp <- function(x) { p <- strsplit(x, " "); vapply(p, function(s) if (length(s) >= 2) paste0(substr(s[1], 1, 1), ". ", paste(s[-1], collapse = " ")) else s[1], "") }

desenhar_camp <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ","); tam <- e$fonte / 3.1; gr <- G$grupo
  if (chave %in% c("campanhas_pontos", "campanhas_especies", "campanhas_curvas", "campanhas_familias", "campanhas_estimadores", "campanhas_dominancia", "campanhas_rarefacao")) forma <- sub("campanhas_", "", chave)
  if (startsWith(chave, "campanhas") && !is.null(G$campanhas)) {
    C <- G$campanhas; cods <- C$resumo$codigo
    if (forma == "rarefacao") {
      d <- C$rarefacao; if (is.null(d) || !nrow(d)) return(invisible()); d$campanha <- factor(d$campanha, levels = cods)
      d$trecho <- ifelse(d$metodo == "Extrapolation", "Extrapolação", "Rarefação")
      obs <- d[d$metodo == "Observed", , drop = FALSE]
      ext <- d[d$metodo != "Rarefaction", , drop = FALSE]; int <- d[d$metodo != "Extrapolation", , drop = FALSE]
      return(gg(ggplot2::ggplot(d, ggplot2::aes(m, qD, colour = campanha, fill = campanha)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), alpha = .15, colour = NA) +
        ggplot2::geom_line(data = int, linewidth = e$linha * 1.4) + ggplot2::geom_line(data = ext, linewidth = e$linha * 1.2, linetype = 2) +
        ggplot2::geom_point(data = obs, size = e$ponto * 2) +
        ggplot2::geom_text(data = obs, ggplot2::aes(label = paste0(campanha, ": ", round(qD))), vjust = -1, family = e$familia, size = tam, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = setNames(pal[seq_along(cods)], cods), name = "Campanha") + ggplot2::scale_fill_manual(values = setNames(pal[seq_along(cods)], cods), name = "Campanha") +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        titulos_gg(e, chave, paste("Rarefação e extrapolação de cada campanha:", gr), "Número de indivíduos", "Espécies",
                   "Linha cheia: rarefação até a amostra real (ponto). Tracejada: extrapolação. Faixa: intervalo de 95% (iNEXT, 100 réplicas).") + tema_gg(e), w, h))
    }
    if (forma == "estimadores") {
      d <- C$estimadores; if (is.null(d) || !nrow(d)) return(invisible())
      d$estimador <- factor(d$estimador, levels = unique(d$estimador)); d$campanha <- factor(d$campanha, levels = cods)
      return(gg(ggplot2::ggplot(d, ggplot2::aes(estimador, valor, fill = campanha)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .72, na.rm = TRUE) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = valor - 1.96 * ep, ymax = valor + 1.96 * ep), position = ggplot2::position_dodge(.8), width = .2, colour = k$tinta2, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = fmt1(valor), y = 0), position = ggplot2::position_dodge(.8), vjust = -.6, family = e$familia, size = tam * .9, colour = "white", fontface = "bold", na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = setNames(pal[seq_along(cods)], cods), name = "Campanha") + ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .15))) +
        titulos_gg(e, chave, paste("Riqueza observada e estimada em cada campanha:", gr), NULL, "Número de espécies",
                   "Barra preta = intervalo de 95% (1,96 x erro padrão). Unidades: pontos visitados em cada campanha.") + tema_gg(e), w, h))
    }
    if (forma == "dominancia") {
      d <- C$dominancia; if (is.null(d) || !nrow(d)) return(invisible()); d$campanha <- factor(d$campanha, levels = cods)
      top <- do.call(rbind, lapply(split(d, d$campanha), function(q) head(q, 1)))   # só a mais abundante de cada campanha: nomes não se sobrepõem
      return(gg(ggplot2::ggplot(d, ggplot2::aes(posicao, relativa, colour = campanha)) + ggplot2::geom_line(linewidth = e$linha * 1.3) + ggplot2::geom_point(size = e$ponto * 1.3) +
        ggplot2::geom_text(data = top, ggplot2::aes(label = abrev_camp(especie)), hjust = -.12, vjust = -.4, family = e$familia, fontface = "italic", size = tam * .85, show.legend = FALSE) +
        ggplot2::scale_y_log10(labels = function(x) paste0(formatC(100 * x, format = "fg", digits = 2, decimal.mark = ","), "%")) +
        ggplot2::scale_colour_manual(values = setNames(pal[seq_along(cods)], cods), name = "Campanha") +
        ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .12))) +
        titulos_gg(e, chave, paste("Curva de dominância de cada campanha:", gr), "Ranque da espécie (1 = mais abundante)", "Abundância relativa (escala log)",
                   paste(sprintf("%s: %s (%s%%)", cods, C$resumo$dominante, vapply(cods, function(c) { q <- d[d$campanha == c, ]; if (nrow(q)) formatC(100 * q$relativa[1], format = "f", digits = 0) else "-" }, "")), collapse = "; ")) + tema_gg(e), w, h))
    }
    if (forma == "curvas") {
      d <- C$curvas; if (is.null(d) || !nrow(d)) return(invisible())
      cc <- cores_camp(cods, pal); d$campanha <- factor(d$campanha, levels = cods)
      return(curva_estimadores_gg(d, cc, e, k, gg, w, h, titulos_gg(e, chave, paste("Curva de acumulação de espécies em cada campanha:", gr), "Pontos amostrados", "Riqueza de espécies")))
    }
    if (forma == "familias") {
      Fm <- C$familias; d <- do.call(rbind, lapply(cods, function(c) data.frame(familia = Fm$familia, campanha = c, S = Fm[[paste0("S_", c)]])))
      d$familia <- factor(d$familia, levels = rev(sort(unique(Fm$familia)))); d$campanha <- factor(d$campanha, levels = rev(cods))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(S, familia, fill = campanha)) + ggplot2::geom_col(position = ggplot2::position_dodge(width = .75), width = .7) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(S > 0, S, "")), position = ggplot2::position_dodge(width = .75), hjust = -.3, family = e$familia, size = tam * .9, colour = k$tinta) +
        ggplot2::scale_fill_manual(values = setNames(pal[seq_along(cods)], cods), breaks = cods, name = "Campanha") + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
        titulos_gg(e, chave, paste("Espécies por família em cada campanha:", gr), "Número de espécies", NULL, NULL) + tema_gg(e), w, max(h * .7, 1.4 + .32 * nrow(Fm))))
    }
    if (forma == "pontos") {
      d <- do.call(rbind, lapply(cods, function(c) data.frame(ponto = C$por_ponto$ponto, campanha = c, S = C$por_ponto[[paste0("S_", c)]])))
      com <- unique(d$ponto[!is.na(d$S) & d$S > 0]); d <- d[d$ponto %in% com, ]   # ponto sem registro em nenhuma campanha não aparece
      d$ponto <- factor(d$ponto, levels = rev(C$por_ponto$ponto[C$por_ponto$ponto %in% com]))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(S, ponto)) + ggplot2::geom_line(ggplot2::aes(group = ponto), colour = k$tinta2, linewidth = e$linha) +
        ggplot2::geom_point(ggplot2::aes(colour = campanha), size = e$ponto * 2) +
        ggplot2::geom_text(ggplot2::aes(label = S, colour = campanha), vjust = -1.1, family = e$familia, size = tam, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = pal, name = "Campanha") + ggplot2::scale_x_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .12))) +
        titulos_gg(e, chave, paste("Riqueza de cada ponto nas campanhas:", gr), "Espécies", NULL,
                   paste("Jaccard entre campanhas por ponto:", paste(sprintf("%s %s", C$por_ponto$ponto, fmt1(C$por_ponto$jaccard, 2)), collapse = "; "))) + tema_gg(e)))
    }
    if (forma == "especies") {
      E <- C$especies; d <- do.call(rbind, lapply(cods, function(c) data.frame(especie = E$especie, familia = E$familia, campanha = c, n = E[[c]])))
      d <- d[d$n > 0, ]; d$especie <- factor(d$especie, levels = rev(E$especie))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(campanha, especie)) + ggplot2::geom_point(ggplot2::aes(size = n, colour = campanha), show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = n), nudge_x = .28, hjust = 0, family = e$familia, size = tam * .9, colour = k$tinta) +
        ggplot2::scale_size_area(max_size = 9) + ggplot2::scale_colour_manual(values = pal) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")) +
        titulos_gg(e, chave, paste("Espécies registradas em cada campanha:", gr), NULL, NULL, "Tamanho do círculo = número de indivíduos.") + tema_gg(e) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic")), w * .8, max(h, .22 * nrow(E) + 1.2)))
    }
    # 24/11/2026 (pedido do usuário): uma figura para cada parte, cada espécie numa linha com a abundância de cada campanha:
    #   "campanhas" = espécies em mais de uma campanha (barras lado a lado); "campanhas_so_<i>" = só na campanha i
    E <- C$especies; if (is.null(E) || !nrow(E)) return(invisible())
    ab <- as.matrix(E[, cods, drop = FALSE]); ab[is.na(ab)] <- 0; em <- ab > 0
    cc <- cores_camp(cods, pal)
    mi <- regmatches(chave, regexec("^campanhas_so_([0-9]+)$", chave))[[1]]
    if (length(mi)) { i <- as.integer(mi[2]); if (i > length(cods)) return(invisible()); sel <- em[, i] & rowSums(em) == 1
      tit <- paste("Espécies registradas só na campanha", cods[i]) } else { sel <- rowSums(em) >= 2
      tit <- if (length(cods) == 2) "Espécies registradas nas duas campanhas" else "Espécies registradas em mais de uma campanha" }
    if (!any(sel)) return(invisible())
    Es <- E[sel, , drop = FALSE]; abs_ <- ab[sel, , drop = FALSE]; o <- order(-rowSums(abs_), Es$especie)
    d <- do.call(rbind, lapply(seq_along(cods), function(j) data.frame(especie = Es$especie, campanha = cods[j], n = abs_[, j], stringsAsFactors = FALSE)))
    d <- d[d$n > 0, ]
    d$especie <- factor(d$especie, levels = rev(Es$especie[o])); d$campanha <- factor(d$campanha, levels = cods)
    d$ordem <- factor(d$campanha, levels = rev(cods))   # primeira campanha (chuva) em cima no par de barras
    dois <- !length(mi)
    p <- ggplot2::ggplot(d, ggplot2::aes(n, especie, fill = campanha, group = ordem)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(.8, preserve = "single"), width = if (dois) .75 else .6) +
      ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_dodge(.8, preserve = "single"), hjust = -.25, family = e$familia, fontface = "bold", size = tam * .72, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cc, name = NULL) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
      titulos_gg(e, chave, paste0(tit, ": ", gr), "Número de indivíduos", NULL, NULL) + tema_gg(e) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic", colour = k$tinta, size = ggplot2::rel(.82)), panel.grid.major.y = ggplot2::element_blank())
    if (!dois) p <- p + ggplot2::theme(legend.position = "none")
    return(gg(p, w, max(h * .6, (if (dois) .34 else .24) * nrow(Es) + 1.3)))
  }
  if (chave == "nmds" && !is.null(G$nmds$pontos)) {
    d <- G$nmds$pontos; if (is.null(d$grupo)) d$grupo <- "Pontos"; d$grupo <- as.character(d$grupo)
    nota <- sprintf("Stress = %s.%s", fmt1(G$nmds$stress, 3), if (!is.null(G$permanova$R2)) sprintf(" PERMANOVA: R² = %s; p = %s.", fmt1(G$permanova$R2, 2), fmt1(G$permanova$p, 3)) else "")
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y))
    if (forma == "grupos") {
      cas <- do.call(rbind, lapply(split(d, d$grupo), function(q) if (nrow(q) >= 3) q[grDevices::chull(q$x, q$y), ] else q))
      p <- p + ggplot2::geom_polygon(data = cas, ggplot2::aes(fill = grupo, colour = grupo), alpha = .12, linewidth = e$linha * .7, show.legend = FALSE)
    }
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = k$grade, linetype = 3) + ggplot2::geom_vline(xintercept = 0, colour = k$grade, linetype = 3) +
      ggplot2::geom_point(ggplot2::aes(colour = grupo, shape = grupo), size = e$ponto * 1.8) +
      ggplot2::geom_text(ggplot2::aes(label = ponto), vjust = -1, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_colour_manual(values = pal, name = NULL) + ggplot2::scale_fill_manual(values = pal) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .15)) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .15)) +
      ggplot2::labs(shape = NULL) + ggplot2::coord_equal() +
      titulos_gg(e, chave, paste("Ordenação NMDS (Bray-Curtis, raiz):", gr), "NMDS 1", "NMDS 2", nota) + tema_gg(e)
    return(gg(p, w, h * 1.1))
  }
  invisible()
}
