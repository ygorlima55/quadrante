# Quadrante: figuras da descritiva em colunas, atributos, curvas juntas, iNEXT explicado e diversidade por ponto.
# - Descritiva em colunas com rótulos: famílias, famílias em cada ponto (uma coluna por família), forma de registro.
# - Um gráfico para cada atributo das espécies.
# - Curva com Observada + Jackknife 1 + Bootstrap juntas.
# - iNEXT com legenda que diz o que é cada linha, e linhas do observado e dos estimadores.
# - Diversidade por ponto em números de Hill e índices com valores escritos.


# ---- hachura (Direção C): faixas de confiança e segunda série com listras diagonais (pacote ggpattern) ----
# Sem o ggpattern instalado, volta à faixa de cor transparente.
tem_hachura <- function(e = NULL) identical(e$tema_r, "sintese") && requireNamespace("ggpattern", quietly = TRUE)   # só no tema da Direção C
faixa_ic <- function(e, data, cores, grupo = NULL, x = "x") {
  if (!isTRUE(e$faixa) || is.null(data) || !nrow(data)) return(NULL)
  grs <- if (is.null(grupo)) list(data) else split(data, as.character(data[[grupo]]))
  lapply(seq_along(grs), function(i) {
    d <- grs[[i]]; d <- d[order(d[[x]]), , drop = FALSE]
    cor <- if (is.null(grupo)) unname(cores[1]) else unname(cores[[names(grs)[i]]] %||% cores[1])
    d$.x <- d[[x]]
    if (tem_hachura(e)) ggpattern::geom_ribbon_pattern(data = d, mapping = ggplot2::aes(x = .x, ymin = li, ymax = ls), inherit.aes = FALSE,
      fill = grDevices::adjustcolor(cor, .07), colour = NA, pattern = "stripe", pattern_angle = 45, pattern_density = .12,
      pattern_spacing = .014, pattern_fill = grDevices::adjustcolor(cor, .55), pattern_colour = NA)
    else ggplot2::geom_ribbon(data = d, mapping = ggplot2::aes(x = .x, ymin = li, ymax = ls), inherit.aes = FALSE, fill = cor, alpha = .12, colour = NA)
  })
}

ATRIBUTOS <- c(endemismo = "Distribuição (endemismo)", habito = "Hábito", dependencia = "Dependência de ambiente florestal",
               sensibilidade = "Sensibilidade a alterações", salve = "Status nacional (SALVE)", iucn = "Status global (IUCN)")

catalogo_novas <- function() c(
  list(
    familias_pontos = list(nome = "Espécies de cada família por ponto", padrao = "colunas", formas = list(
      colunas = "Uma coluna por família, com valores", empilhado = "Colunas empilhadas com valores")),
    # 18/11/2026: mais descritiva
    composicao_familias = list(nome = "Composição por família", padrao = "colunas", formas = list(
      colunas = "Colunas: espécies por família, com número e %", rosca = "Rosca: % das espécies em cada família (melhor com até 5 famílias)")),
    especies_abundantes = list(nome = "Espécies mais abundantes", padrao = "todas", formas = list(
      todas = "Todas as espécies, da mais para a menos abundante", top10 = "Só as 10 mais abundantes")),
    exclusivas_pontos = list(nome = "Espécies exclusivas de cada ponto", padrao = "barras", formas = list(
      barras = "Barras com o número e o nome das exclusivas", colunas = "Colunas: exclusivas e compartilhadas em cada ponto")),
    curva_jack1_boot = list(nome = "Curvas: Observada, Jackknife 1 e Bootstrap", padrao = "limpa", formas = list(
      limpa = "Limpa: só a curva, a faixa e a legenda no canto",
      faixas = "As três curvas com a faixa sombreada de cada uma (só os pontos amostrados)",
      inext = "Estilo iNEXT: curvas com faixa e linhas pontilhadas do valor final",
      linhas = "Três curvas com valores", final = "Só o valor final de cada uma"))
  ),
  setNames(lapply(names(ATRIBUTOS), function(a) list(nome = paste("Atributo:", ATRIBUTOS[[a]]), padrao = "colunas", formas = list(
    colunas = "Colunas com número e %", barras = "Barras deitadas com número e %"))), paste0("atributo_", names(ATRIBUTOS)))
)

# legenda dentro do gráfico, num canto vazio (19/11/2026: legenda em cima tomava espaço à toa)
leg_dentro <- function(k, canto = c(.98, .03)) ggplot2::theme(legend.position = "inside", legend.position.inside = canto, legend.justification = canto,
  legend.direction = "vertical", legend.box = "vertical", legend.background = ggplot2::element_rect(fill = grDevices::adjustcolor(k$fundo, .92), colour = NA),
  legend.margin = ggplot2::margin(6, 8, 6, 8), legend.key.size = grid::unit(.95, "lines"), legend.text = ggplot2::element_text(size = ggplot2::rel(.92)))
# cor fixa para cada família (a mesma em todas as figuras)
cores_familias <- function(fams, pal) {
  u <- sort(unique(fams)); n <- length(u)
  extra <- if (n > length(pal)) grDevices::adjustcolor(pal, red.f = .7, green.f = .7, blue.f = .7) else NULL
  setNames(rep(c(pal, extra), length.out = n), u)
}

desenhar_novas <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  pc <- function(x) paste0(fmt1(100 * x, 0), "%")
  D <- G$descritiva; tam <- e$fonte / 3.1
  rotx <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1))

  if (chave == "familias_pontos" && !is.null(D)) {
    le <- D$especies; pts <- D$pontos$ponto
    d <- do.call(rbind, lapply(pts, function(p) { z <- le[le[[p]] > 0, ]; if (!nrow(z)) return(NULL)
      t <- table(z$familia); data.frame(ponto = p, familia = names(t), n = as.integer(t)) }))
    d$ponto <- factor(d$ponto, levels = pts); cf <- cores_familias(le$familia, pal)
    if (forma == "empilhado") return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, n, fill = familia)) +
      ggplot2::geom_col(width = .66, colour = k$fundo, linewidth = .4) +
      ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam, colour = "white", fontface = "bold") +
      ggplot2::stat_summary(fun = sum, ggplot2::aes(label = ggplot2::after_stat(y), group = ponto, fill = NULL), geom = "text", vjust = -.5, family = e$familia, size = tam * 1.1, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
      titulos_gg(e, chave, "Espécies por família em cada ponto", NULL, "Número de espécies", "Número em cima da coluna = total de espécies do ponto.") + tema_gg(e)))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, n, fill = familia)) +
      ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single", padding = .12), width = .85) +
      ggplot2::geom_text(ggplot2::aes(label = n, group = familia), position = ggplot2::position_dodge2(width = .85, preserve = "single", padding = .12),
                         vjust = -.4, family = e$familia, size = tam * .95, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12)), breaks = function(l) unique(floor(pretty(l)))) +
      titulos_gg(e, chave, "Espécies por família em cada ponto", NULL, "Número de espécies") + tema_gg(e), w * 1.15, h))
  }

  abrev <- function(x) sub("^(\\w)\\w*\\s+", "\\1. ", x)
  leg_cima <- ggplot2::theme(legend.position = "top", legend.direction = "horizontal", legend.justification = "left")

  if (chave == "composicao_familias" && !is.null(D)) {
    tf <- D$familias; cf <- cores_familias(D$especies$familia, pal)
    tf$familia <- factor(tf$familia, levels = tf$familia); tf$rot <- paste0(tf$riqueza, " (", pc(tf$riqueza_pct), ")")
    if (forma == "colunas") return(gg(ggplot2::ggplot(tf, ggplot2::aes(familia, riqueza, fill = familia)) + ggplot2::geom_col(width = .6, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = rot), vjust = -.5, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cf) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14)), breaks = function(l) unique(floor(pretty(l)))) +
      titulos_gg(e, chave, "Espécies por família", NULL, "Número de espécies") + tema_gg(e) + (if (nrow(tf) > 5) rotx)))
    tf$rot_fora <- ifelse(tf$riqueza_pct >= .06, tf$rot, "")
    nl <- ceiling(nrow(tf) / 3)
    return(gg(ggplot2::ggplot(tf, ggplot2::aes(x = 2, y = riqueza, fill = familia)) +
      ggplot2::geom_col(width = 1, colour = k$fundo, linewidth = .8) +
      ggplot2::geom_text(ggplot2::aes(label = rot_fora), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam * .95, colour = "white", fontface = "bold") +
      ggplot2::annotate("text", x = .5, y = 0, label = paste0(sum(tf$riqueza), "
espécies"), family = e$familia, size = tam * 1.4, colour = k$tinta, lineheight = .9) +
      ggplot2::coord_polar(theta = "y") + ggplot2::xlim(.5, 2.5) +
      ggplot2::scale_fill_manual(values = cf, name = NULL, labels = paste0(tf$familia, " (", tf$riqueza, ")")) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = nl, byrow = TRUE)) +
      titulos_gg(e, chave, "Composição por família", NULL, NULL) + tema_gg(e) + leg_cima +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                     legend.justification = "center"), w, h * 1.15))
  }

  if (chave == "especies_abundantes" && !is.null(D)) {
    le <- D$especies; le <- le[le$total > 0, ]; le <- le[order(-le$total, le$especie), ]
    if (forma == "top10") le <- utils::head(le, 10)
    tot <- sum(D$especies$total); cf <- cores_familias(D$especies$familia, pal)
    le$especie <- factor(le$especie, levels = rev(le$especie)); le$rot <- paste0(le$total, " (", fmt1(100 * le$total / tot, 1), "%)")
    return(gg(ggplot2::ggplot(le, ggplot2::aes(total, especie, fill = familia)) + ggplot2::geom_col(width = .68) +
      ggplot2::geom_text(ggplot2::aes(label = rot), hjust = -.08, family = e$familia, size = tam * .9, colour = k$tinta) +
      ggplot2::scale_fill_manual(values = cf, name = NULL) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .16))) +
      ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
      titulos_gg(e, chave, if (forma == "top10") "As 10 espécies mais abundantes" else "Abundância por espécie", "Número de indivíduos", NULL) +
      tema_gg(e) + leg_dentro(k) + ggplot2::theme(axis.text.y = ggplot2::element_text(face = "italic", colour = k$tinta), panel.grid.major.y = ggplot2::element_blank(),
                                             panel.grid.major.x = if (isTRUE(e$grade)) ggplot2::element_line(colour = k$grade, linewidth = .4) else ggplot2::element_blank()),
      w, max(3, .26 * nrow(le) + 1.5)))
  }

  if (chave == "exclusivas_pontos" && !is.null(D)) {
    le <- D$especies; pts <- D$pontos$ponto
    d <- do.call(rbind, lapply(pts, function(p) { ex <- le$especie[le[[p]] > 0 & le$n_pontos == 1]; tot <- sum(le[[p]] > 0)
      nm <- abrev(sort(ex)); linhas <- split(nm, ceiling(seq_along(nm) / 4))   # 4 nomes por linha: a lista não sai da figura
      data.frame(ponto = p, exclusivas = length(ex), compartilhadas = tot - length(ex), nomes = paste(vapply(linhas, paste, "", collapse = ", "), collapse = ",\n"), stringsAsFactors = FALSE) }))
    d <- d[d$exclusivas + d$compartilhadas > 0, , drop = FALSE]; pts <- as.character(d$ponto)   # ponto sem registro não entra
    d$ponto <- factor(d$ponto, levels = rev(pts))
    if (forma == "colunas") {
      dd <- rbind(data.frame(ponto = d$ponto, tipo = "Exclusivas do ponto", n = d$exclusivas), data.frame(ponto = d$ponto, tipo = "Também em outros pontos", n = d$compartilhadas))
      dd$ponto <- factor(dd$ponto, levels = pts); dd$tipo <- factor(dd$tipo, levels = c("Também em outros pontos", "Exclusivas do ponto"))
      return(gg(ggplot2::ggplot(dd, ggplot2::aes(ponto, n, fill = tipo)) + ggplot2::geom_col(width = .62, colour = k$fundo, linewidth = .4) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(n > 0, n, "")), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam, colour = "white", fontface = "bold") +
        ggplot2::scale_fill_manual(values = c("Exclusivas do ponto" = pal[2], "Também em outros pontos" = pal[1]), name = NULL, breaks = c("Exclusivas do ponto", "Também em outros pontos")) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .06)), breaks = function(l) unique(floor(pretty(l)))) +
        titulos_gg(e, chave, "Exclusivas e compartilhadas por ponto", NULL, "Número de espécies") + tema_gg(e) + leg_cima))
    }
    m <- max(1, d$exclusivas); passo <- m * .06
    return(gg(ggplot2::ggplot(d, ggplot2::aes(exclusivas, ponto)) + ggplot2::geom_col(fill = pal[2], width = .55) +
      ggplot2::geom_text(ggplot2::aes(x = exclusivas + passo, label = exclusivas), hjust = 0, family = e$familia, size = tam, colour = k$tinta, fontface = "bold") +
      ggplot2::geom_text(ggplot2::aes(x = exclusivas + passo * 4, label = nomes), hjust = 0, family = e$familia, fontface = "italic", size = tam * .9, colour = k$tinta) +
      ggplot2::scale_x_continuous(limits = c(0, m * 2.6 + 1), expand = ggplot2::expansion(mult = c(0, 0))) +
      titulos_gg(e, chave, "Espécies exclusivas por ponto", NULL, NULL) + tema_gg(e) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.line.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(colour = k$tinta, size = ggplot2::rel(1.05))), w, max(2.6, .45 * length(pts) + 1.4)))
  }

  if (startsWith(chave, "atributo_") && !is.null(D)) {
    a <- sub("^atributo_", "", chave); t <- D$categorias[[a]]; if (is.null(t) || !nrow(t)) return(invisible())
    falta <- sum(t$especies_n[t$classe == "Sem informação"]); t <- t[t$classe != "Sem informação", , drop = FALSE]
    if (!nrow(t)) return(invisible())
    t$especies_pct <- t$especies_n / sum(t$especies_n)
    t$rot <- paste0(t$especies_n, " (", pc(t$especies_pct), ")")
    t$classe <- factor(t$classe, levels = t$classe[order(-t$especies_n)])
    tt <- ATRIBUTOS[[a]]
    if (forma == "barras") return(gg(ggplot2::ggplot(t, ggplot2::aes(especies_n, stats::reorder(classe, especies_n))) + ggplot2::geom_col(fill = pal[1], width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = rot), hjust = -.12, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .25))) +
      titulos_gg(e, chave, tt, "Número de espécies", NULL) + tema_gg(e), w * .85, max(2.4, .55 * nrow(t) + 1.4)))
    return(gg(ggplot2::ggplot(t, ggplot2::aes(classe, especies_n)) + ggplot2::geom_col(fill = pal[1], width = .55) +
      ggplot2::geom_text(ggplot2::aes(label = rot), vjust = -.5, family = e$familia, size = tam * 1.05, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
      titulos_gg(e, chave, tt, NULL, "Número de espécies", paste0(sprintf("%d espécies com a informação; porcentagem sobre elas.", sum(t$especies_n)),
        if (falta) sprintf(" Faltam %d espécies: preencha em Planilha > Conferência.", falta) else "")) + tema_gg(e) +
      (if (nrow(t) > 4 || max(nchar(as.character(t$classe))) > 18) rotx), w * .85, h * .9))
  }

  if (chave == "curva_jack1_boot" && !is.null(G$acumulacao$poolaccum) && forma %in% c("limpa", "faixas"))
    return(curvas_limpas(chave, list(curva_obs_inext(G), curva_estimador(G, "jack1"), curva_estimador(G, "boot")), c("Riqueza observada", "Jackknife 1", "Bootstrap"), c(pal[1], pal[2], pal[4]), e, gg, k, w, h,
                         faixas = forma == "faixas"))
  if (chave == "curva_jack1_boot" && !is.null(G$acumulacao$poolaccum) && forma == "inext") {
    c1 <- G$acumulacao$curva; pool <- G$acumulacao$poolaccum
    zs <- list(data.frame(pontos = c1$pontos, media = c1$riqueza, li = c1$li, ls = c1$ls),
               pool[pool$estimador == "jack1", c("pontos", "media", "li", "ls")], pool[pool$estimador == "boot", c("pontos", "media", "li", "ls")])
    zs <- list(curva_obs_inext(G), curva_estimador(G, "jack1"), curva_estimador(G, "boot"))
    return(curvas_estilo_inext(chave, zs, c("Riqueza observada", "Jackknife 1", "Bootstrap"), c(pal[1], pal[2], pal[4]), G, e, gg, pal, k, w, h))
  }
  if (chave == "curva_jack1_boot" && !is.null(G$acumulacao$poolaccum)) {
    c1 <- G$acumulacao$curva; pool <- G$acumulacao$poolaccum
    d <- rbind(data.frame(serie = "Riqueza observada", pontos = c1$pontos, v = c1$riqueza, li = c1$li, ls = c1$ls),
               data.frame(serie = "Jackknife 1", pontos = pool$pontos[pool$estimador == "jack1"], v = pool$media[pool$estimador == "jack1"], li = pool$li[pool$estimador == "jack1"], ls = pool$ls[pool$estimador == "jack1"]),
               data.frame(serie = "Bootstrap", pontos = pool$pontos[pool$estimador == "boot"], v = pool$media[pool$estimador == "boot"], li = pool$li[pool$estimador == "boot"], ls = pool$ls[pool$estimador == "boot"]))
    d$serie <- factor(d$serie, levels = c("Jackknife 1", "Bootstrap", "Riqueza observada"))
    cs <- c("Jackknife 1" = pal[2], "Bootstrap" = pal[4], "Riqueza observada" = k$tinta)
    fim <- do.call(rbind, lapply(split(d, d$serie), function(z) z[which.max(z$pontos), ]))
    fim$rot <- paste0(fim$serie, ": ", fmt1(fim$v))
    p <- ggplot2::ggplot(d, ggplot2::aes(pontos, v, colour = serie, fill = serie)) +
      faixa_ic(e, d, cs, "serie", "pontos") +
      ggplot2::geom_line(linewidth = e$linha * 1.2) + ggplot2::geom_point(size = e$ponto * 1.2)
    if (forma == "final") p <- p + ggplot2::geom_label(data = fim, ggplot2::aes(label = rot), hjust = -.08, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = unique(d$pontos), expand = ggplot2::expansion(mult = c(.04, .35)))
    else {
      d$vj <- ifelse(d$serie == "Riqueza observada", 1.7, ifelse(d$serie == "Jackknife 1", -.8, .5)); d$hj <- ifelse(d$serie == "Bootstrap", -.35, .5)
      p <- p + ggplot2::geom_text(data = d, ggplot2::aes(label = fmt1(v), vjust = vj, hjust = hj), family = e$familia, size = tam * .95, fontface = "bold", show.legend = FALSE) +
        ggplot2::scale_x_continuous(breaks = unique(d$pontos), expand = ggplot2::expansion(mult = c(.05, .12)))
    }
    return(gg(p + ggplot2::scale_colour_manual(values = cs, name = NULL) + ggplot2::scale_fill_manual(values = cs, name = NULL) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, .12))) +
      titulos_gg(e, chave, "Riqueza observada e estimada (Jackknife 1 e Bootstrap)", "Número de pontos amostrais", "Número de espécies",
                 "Observada: vegan::specaccum; estimadores: vegan::poolaccum. Faixas = IC 95%.") + tema_gg(e)))
  }
  invisible()
}

# ---- iNEXT legível: curva de riqueza com observado e estimadores ----
inext_claro <- function(chave, forma, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  por_ind <- chave == "inext_individuos"; z <- if (por_ind) G$inext$abundancia else G$inext$incidencia
  if (is.null(z)) return(invisible())
  z$parte <- ifelse(z$metodo == "Extrapolation", "Extrapolação (projeção além da amostra)", "Rarefação (dentro da amostra real)")
  # o ponto da amostra real também começa a parte tracejada (sem buraco entre as duas)
  liga <- z[z$metodo == "Observed", ]; if (nrow(liga)) { liga$parte <- "Extrapolação (projeção além da amostra)"; z <- rbind(z, liga) }
  inteiro <- ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .04)))
  leg_v <- ggplot2::theme(legend.box = "vertical", legend.box.just = "left", legend.position = "bottom", legend.title.position = "top", legend.justification = "left")
  xlab <- if (por_ind) "Número de indivíduos amostrados" else "Número de pontos amostrais"
  tam <- e$fonte / 3.1; lt <- c("Rarefação (dentro da amostra real)" = "solid", "Extrapolação (projeção além da amostra)" = "22")
  if (forma == "hill") {
    nq <- c("0" = "Riqueza (q = 0)", "1" = "Espécies comuns: Shannon (q = 1)", "2" = "Espécies dominantes: Simpson (q = 2)")
    z$qn <- factor(nq[as.character(z$q)], levels = nq); ob <- z[z$metodo == "Observed" & z$parte != "Extrapolação (projeção além da amostra)", ]
    return(gg(ggplot2::ggplot(z, ggplot2::aes(m, qD, colour = qn, fill = qn)) +
      faixa_ic(e, z[!duplicated(paste(z$qn, z$m)), ], setNames(pal[1:3], levels(z$qn)), "qn", "m") +
      ggplot2::geom_line(ggplot2::aes(linetype = parte, group = interaction(qn, parte)), linewidth = e$linha * 1.1) +
      ggplot2::geom_point(data = ob, size = e$ponto * 1.8, shape = 21, colour = k$fundo, stroke = .8) +
      ggplot2::geom_label(data = ob, ggplot2::aes(label = fmt1(qD)), vjust = -.6, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, fontface = "bold", show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = pal[1:3], name = NULL) + ggplot2::scale_fill_manual(values = pal[1:3], name = NULL) + ggplot2::scale_linetype_manual(values = lt, name = NULL) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1, order = 1), linetype = ggplot2::guide_legend(ncol = 1, order = 2), fill = "none") +
      titulos_gg(e, chave, paste("Diversidade em números de Hill por", if (por_ind) "indivíduos" else "pontos"), xlab, "Número efetivo de espécies",
                 "Círculo = valor da amostra real. Faixas = IC 95% (bootstrap, 200 réplicas). iNEXT.") + tema_gg(e) + leg_v + (if (!por_ind) inteiro), w, h * 1.3))
  }
  z <- z[z$q == 0, ]; ob <- z[z$metodo == "Observed" & z$parte != "Extrapolação (projeção além da amostra)", ]
  a <- if (por_ind) G$inext$assintotico_abund else G$inext$assintotico_inc
  est <- G$acumulacao$estimadores
  ref <- data.frame(nome = character(), v = numeric())
  ref <- rbind(ref, data.frame(nome = "Riqueza observada", v = G$riqueza))
  if (!is.null(a)) ref <- rbind(ref, data.frame(nome = if (por_ind) "Chao 1 (iNEXT)" else "Chao 2 (iNEXT)", v = a$estimado[1]))
  if (!por_ind && !is.null(est)) for (n in c("Jackknife 1", "Bootstrap")) if (n %in% est$estimador) ref <- rbind(ref, data.frame(nome = n, v = est$valor[est$estimador == n]))
  ref$rot <- paste0(ref$nome, " = ", fmt1(ref$v))
  ref$rot <- factor(ref$rot, levels = ref$rot)
  cr <- setNames(c(k$tinta, pal[c(2, 4, 3)])[seq_len(nrow(ref))], levels(ref$rot))
  xmax <- max(z$m)
  return(gg(ggplot2::ggplot(z, ggplot2::aes(m, qD)) +
    faixa_ic(e, z[!duplicated(z$m), ], pal[1], NULL, "m") +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = v, colour = rot), linetype = 3, linewidth = e$linha * .9) +
    ggplot2::geom_line(ggplot2::aes(linetype = parte, group = parte), colour = pal[1], linewidth = e$linha * 1.3) +
    ggplot2::geom_point(data = ob, colour = pal[1], size = e$ponto * 2) +
    ggplot2::geom_label(data = ob, ggplot2::aes(label = paste0("amostra real: ", fmt1(qD, 0), " espécies")), vjust = 1.6, hjust = .1, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, colour = pal[1]) +
    ggplot2::scale_colour_manual(values = cr, name = "Linhas pontilhadas") + ggplot2::scale_linetype_manual(values = lt, name = "Curva de riqueza") +
    (if (!por_ind) inteiro) +
    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = 1, order = 1), colour = ggplot2::guide_legend(ncol = 1, order = 2)) +
    titulos_gg(e, chave, paste("Riqueza por rarefação e extrapolação (por", if (por_ind) "indivíduos)" else "pontos)"), xlab, "Número de espécies",
               "Faixa = IC 95% da curva. Pontilhadas = riqueza observada e estimadores. iNEXT (Chao et al. 2014).") + tema_gg(e) + leg_v, w, h * 1.4))
}


# ---- curvas de acumulação e estimadores no estilo da figura do iNEXT ----
# Linha suavizada (spline monótona) entre os valores de cada número de pontos, faixa do IC 95%,
# ponto grande na amostra real com o valor, e linhas pontilhadas: riqueza observada e valor final de cada estimador.
curvas_estilo_inext <- function(chave, zs, nomes, cores, G, e, gg, pal, k, w, h) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ","); tam <- e$fonte / 3.1
  # cada série: pontos, media, li, ls e, quando vem do iNEXT, "real" = TRUE até a amostra real (depois é extrapolação)
  suave <- function(z, nome) {
    z <- z[order(z$pontos), ]; z <- z[!duplicated(z$pontos), ]
    T <- if (!is.null(z$real)) max(z$pontos[z$real]) else max(z$pontos)
    xs <- seq(min(z$pontos), max(z$pontos), length.out = 160)
    f <- function(y) stats::approx(z$pontos, y, xs, rule = 2, na.rm = FALSE, ties = "ordered")$y
    if (nrow(z) == 1) f <- function(y) rep(y, length(xs))
    yy <- f(z$media)
    data.frame(serie = nome, x = xs, y = yy, li = pmin(f(z$li), yy), ls = pmax(f(z$ls), yy), extra = xs > T + 1e-9, T = T)
  }
  d <- do.call(rbind, Map(suave, zs, nomes)); d$serie <- factor(d$serie, levels = nomes)
  # ponto da amostra real (no fim da parte real) e valor final de cada estimador
  fim <- do.call(rbind, Map(function(z, nome) { T <- if (!is.null(z$real)) max(z$pontos[z$real]) else max(z$pontos); r <- z[z$pontos == T, ][1, ]
    data.frame(serie = nome, x = T, y = r$media) }, zs, nomes)); fim$serie <- factor(fim$serie, levels = nomes)
  tem_extra <- any(d$extra)
  obs <- G$riqueza
  ref <- data.frame(nome = paste0("Riqueza observada = ", obs), v = obs)
  for (i in seq_along(nomes)) if (nomes[i] != "Riqueza observada") ref <- rbind(ref, data.frame(nome = paste0(nomes[i], " com todos os pontos = ", fmt1(fim$y[i])), v = fim$y[i]))
  ref$nome <- factor(ref$nome, levels = ref$nome)
  cs <- setNames(cores, nomes); cr <- setNames(c(k$tinta, cores[nomes != "Riqueza observada"])[seq_len(nrow(ref))], levels(ref$nome))
  um <- length(nomes) == 1
  rot <- if (um) paste0(if (nomes == "Riqueza observada") "amostra real: " else paste0(nomes, " com ", fim$x, " pontos: "), fmt1(fim$y, if (nomes == "Riqueza observada") 0 else 1), " espécies") else paste0(fim$serie, ": ", fmt1(fim$y))
  real <- d[!d$extra | abs(d$x - d$T) < 1e-9, ]; proj <- d[d$extra | abs(d$x - d$T) < 1e-6, ]
  fonte_obs <- any(vapply(zs, function(z) !is.null(z$real), TRUE))
  nota <- paste0("Faixa = IC 95%",
    if (fonte_obs) "; riqueza observada pelo iNEXT por pontos (bootstrap), tracejado = extrapolação" else "",
    if (any(nomes != "Riqueza observada")) "; estimadores: erro padrão do vegan::specpool somado à variação entre combinações de pontos" else "", ".")
  nota <- paste(strwrap(nota, width = 105), collapse = "
")
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    faixa_ic(e, d, cs, "serie", "x") +
    ggplot2::geom_hline(data = ref, ggplot2::aes(yintercept = v, linetype = nome), colour = cr[as.character(ref$nome)], linewidth = e$linha * .9) +
    ggplot2::geom_line(data = real, ggplot2::aes(colour = serie, group = serie), linewidth = e$linha * 1.3) +
    { if (tem_extra) ggplot2::geom_line(data = proj, ggplot2::aes(colour = serie, group = serie), linewidth = e$linha * 1.3, linetype = "22") } +
    ggplot2::geom_point(data = fim, ggplot2::aes(colour = serie), size = e$ponto * 2) +
    { if (um) ggplot2::geom_label(data = fim, ggplot2::aes(label = rot, colour = serie), vjust = 1.9, hjust = if (tem_extra) .5 else .95, family = e$familia, size = tam, fill = k$fundo, linewidth = 0, show.legend = FALSE) } +
    ggplot2::scale_colour_manual(values = cs, name = if (um) NULL else "Curvas") + ggplot2::scale_fill_manual(values = cs) +
    ggplot2::scale_linetype_manual(values = setNames(rep("dotted", nrow(ref)), levels(ref$nome)), name = "Linhas pontilhadas",
      guide = ggplot2::guide_legend(ncol = 1, override.aes = list(colour = unname(cr), linewidth = e$linha))) +
    ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.02, .06))) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, .08))) +
    ggplot2::guides(colour = if (um) "none" else ggplot2::guide_legend(ncol = 1, order = 1)) +
    titulos_gg(e, chave, if (um) paste(nomes, "por número de pontos") else "Riqueza observada e estimada por número de pontos", "Número de pontos amostrais", "Número de espécies", nota) +
    tema_gg(e) + ggplot2::theme(legend.box = "vertical", legend.box.just = "left", legend.position = "bottom", legend.title.position = "top", legend.justification = "left")
  gg(p, w, h * (if (um) 1.25 else 1.6))
}

# riqueza observada pela curva do iNEXT por pontos (mesma média da acumulação, faixa de bootstrap e extrapolação)
curva_obs_inext <- function(G) {
  z <- G$inext$incidencia
  if (is.null(z)) { c1 <- G$acumulacao$curva; return(data.frame(pontos = c1$pontos, media = c1$riqueza, li = c1$li, ls = c1$ls)) }
  z <- z[z$q == 0, ]
  data.frame(pontos = z$m, media = z$qD, li = z$li, ls = z$ls, real = z$metodo != "Extrapolation")
}
# estimador com a faixa do erro padrão (cai para a faixa do poolaccum se não houver)
curva_estimador <- function(G, est) {
  pe <- G$acumulacao$poolaccum_ep
  # o vegan::specpool não dá erro padrão para o Jackknife 2: sem faixa, usa a do poolaccum (variação entre as ordens dos pontos)
  if (!is.null(pe) && any(pe$estimador == est & is.finite(pe$li) & is.finite(pe$ls))) return(pe[pe$estimador == est, c("pontos", "media", "li", "ls")])
  pool <- G$acumulacao$poolaccum; pool[pool$estimador == est, c("pontos", "media", "li", "ls")]
}


# ---- curvas limpas (como as figuras de rarefação dos artigos): só a curva, a faixa estreita e a legenda dentro do gráfico ----
# Sem título, subtítulo, nota, linhas pontilhadas nem rótulos. Linha cheia até a amostra real; tracejada na extrapolação.
# rótulos do valor final ao lado da curva, afastados para não se sobreporem (mínimo de 5% da altura do gráfico)
rotulos_afastados <- function(fim, folga = .05) {
  o <- order(fim$y); y <- fim$y[o]; d <- folga * max(1, diff(range(c(0, fim$ls, fim$y)), na.rm = TRUE))
  for (i in seq_along(y)[-1]) if (y[i] - y[i - 1] < d) y[i] <- y[i - 1] + d
  fim$y_rot <- NA; fim$y_rot[o] <- y; fim
}
curvas_limpas <- function(chave, zs, nomes, cores, e, gg, k, w, h, xlab = "Número de pontos amostrais", faixas = FALSE) {
  varias <- length(zs) > 1
  # faixas = TRUE (18/11/2026): várias curvas, cada uma com a sua faixa sombreada, só nos pontos amostrados (sem projeção)
  if (faixas) zs <- lapply(zs, function(z) { if (!is.null(z$real)) z <- z[z$real, , drop = FALSE]; z$real <- NULL; z })
  n_amostra <- max(unlist(lapply(zs, function(z) if (!is.null(z$real)) z$pontos[z$real] else z$pontos)))
  pts <- do.call(rbind, Map(function(z, nome) { if (!is.null(z$real)) z <- z[z$real, , drop = FALSE]; data.frame(serie = nome, x = z$pontos, y = z$media) }, zs, nomes))
  pts$serie <- factor(pts$serie, levels = nomes)
  suave <- function(z, nome) {
    z <- z[order(z$pontos), ]; z <- z[!duplicated(z$pontos), ]
    T <- if (!is.null(z$real)) max(z$pontos[z$real]) else max(z$pontos)
    xs <- sort(unique(c(z$pontos, seq(min(z$pontos), max(z$pontos), length.out = 200))))
    f <- function(y) stats::approx(z$pontos, y, xs, rule = 2, na.rm = FALSE, ties = "ordered")$y
    if (nrow(z) == 1) f <- function(y) rep(y, length(xs))
    yy <- f(z$media)
    data.frame(serie = nome, x = xs, y = yy, li = pmin(f(z$li), yy), ls = pmax(f(z$ls), yy), T = T)
  }
  d <- do.call(rbind, Map(suave, zs, nomes)); d$serie <- factor(d$serie, levels = nomes)
  d$parte <- factor(ifelse(d$x > d$T + 1e-9, "Extrapolação", "Rarefação"), levels = c("Rarefação", "Extrapolação"))
  rot_partes <- c("Rarefação" = "Interpolação", "Extrapolação" = "Extrapolação")
  lig <- d[d$x == d$T & d$serie %in% d$serie[d$x > d$T], ]; lig$parte <- rep("Extrapolação", nrow(lig)); d2 <- rbind(d, lig)
  tem_extra <- any(d$parte == "Extrapolação")
  fim <- do.call(rbind, lapply(split(d, d$serie), function(q) q[which.min(abs(q$x - q$T)), ]))
  cs <- setNames(cores, nomes)
  fmt_v <- function(v) ifelse(abs(v - round(v)) < .05, formatC(round(v), format = "d"), formatC(v, format = "f", digits = 1, decimal.mark = ","))
  fim_proj <- do.call(rbind, lapply(split(d, d$serie), function(q) q[which.max(q$x), ])); fim_proj <- fim_proj[fim_proj$x > fim_proj$T + 1e-9 & as.integer(fim_proj$serie) == 1, ]   # só a projeção da observada (os estimadores ficam retos)
  p <- ggplot2::ggplot(d2, ggplot2::aes(x, y, colour = serie, fill = serie)) +
    { if (isTRUE(e$faixa) && (!varias || faixas)) ggplot2::geom_ribbon(data = d, ggplot2::aes(ymin = li, ymax = ls, group = serie, alpha = serie), colour = NA, show.legend = FALSE) } +
    ggplot2::scale_alpha_manual(values = setNames(if (varias) c(.30, rep(.16, length(nomes) - 1)) else .22, nomes), guide = "none") +
    ggplot2::geom_line(ggplot2::aes(linetype = parte, group = interaction(serie, parte)), linewidth = e$linha * 1.1) +
    { if (varias) ggplot2::geom_point(data = fim, ggplot2::aes(shape = serie), size = e$ponto * (if (faixas) 1.5 else 2.2)) else ggplot2::geom_point(data = fim, size = e$ponto * 1.3, shape = 21, stroke = .8, show.legend = FALSE) } +
    { if (varias && !faixas) ggplot2::geom_text(data = fim, ggplot2::aes(label = ifelse(as.integer(serie) == 1, fmt_v(y), formatC(y, format = "f", digits = 1, decimal.mark = ",")), colour = serie), vjust = ifelse(as.integer(fim$serie) == 1, 2.2, -1.5),
                                     family = e$familia, size = e$fonte / 3.1, fontface = "bold", show.legend = FALSE) } +
    { if (varias && faixas) ggplot2::geom_text(data = rotulos_afastados(fim), ggplot2::aes(x = x, y = y_rot, label = ifelse(as.integer(serie) == 1, fmt_v(y), formatC(y, format = "f", digits = 1, decimal.mark = ",")), colour = serie),
                                     hjust = 0, nudge_x = .1, family = e$familia, size = e$fonte / 3.1, fontface = "bold", show.legend = FALSE) } +
    { if (varias && tem_extra) ggplot2::geom_text(data = fim_proj, ggplot2::aes(label = fmt_v(y), colour = serie), hjust = -.15, family = e$familia, size = e$fonte / 3.3, show.legend = FALSE) } +
    { if (varias) ggplot2::scale_shape_manual(values = setNames(c(16, 17, 15, 18, 1, 2)[seq_along(nomes)], nomes), name = "Curva") } +
    ggplot2::scale_colour_manual(values = cs, name = if (varias) "Curva" else NULL) + ggplot2::scale_fill_manual(values = cs, name = if (varias) "Curva" else NULL) +
    ggplot2::scale_linetype_manual(values = c("Rarefação" = "solid", "Extrapolação" = "22"), labels = rot_partes, name = "Método", drop = TRUE,
                                   guide = if (tem_extra) ggplot2::guide_legend(order = 1, override.aes = list(colour = k$tinta, fill = NA, linewidth = .8)) else "none") +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 2, override.aes = list(linetype = "solid", size = e$ponto * 1.1, fill = NA)),
                    fill = if (varias) "none" else ggplot2::guide_legend(order = 2), shape = ggplot2::guide_legend(order = 2, override.aes = list(size = e$ponto * 1.1))) +
    ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l))), expand = ggplot2::expansion(mult = c(.01, if (varias) .07 else .03))) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, if (varias) .1 else .05))) +
    ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL, x = rot(e, chave, "x", xlab), y = rot(e, chave, "y", "Número de espécies")) +
    ggplot2::theme_classic(e$fonte, e$familia) +
    ggplot2::theme(text = ggplot2::element_text(colour = k$tinta), axis.text = ggplot2::element_text(colour = k$tinta2),
      axis.line = ggplot2::element_line(colour = k$tinta2, linewidth = .4), axis.ticks = ggplot2::element_line(colour = k$tinta2, linewidth = .35),
      plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA), panel.background = ggplot2::element_rect(fill = k$fundo, colour = NA),
      legend.position = c(.98, .04), legend.justification = c(1, 0), legend.background = ggplot2::element_rect(fill = grDevices::adjustcolor(k$fundo, .85), colour = NA),
      legend.key = ggplot2::element_rect(fill = NA, colour = NA), legend.key.width = grid::unit(1.4, "lines"), legend.spacing.y = grid::unit(2, "pt"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(.85)), plot.margin = ggplot2::margin(12, 16, 8, 10))
  gg(p, w, h)
}
