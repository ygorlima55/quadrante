# Quadrante: figuras da descritiva, curvas de cada estimador (com valores), dominância com nomes,
# similaridade didática, diversidade beta e Shannon com IC. Ligadas ao catálogo de saida.R.

catalogo_extra <- function() c(catalogo_camp(), catalogo_novas(), catalogo_fito(), if (exists("catalogo_est")) catalogo_est(), list(
  familias = list(nome = "Riqueza e abundância por família", padrao = "lado_a_lado", formas = list(
    lado_a_lado = "Um gráfico: riqueza e abundância lado a lado (% do total)", colunas = "Dois painéis: riqueza em cima, abundância embaixo", duas = "Barras deitadas, riqueza e abundância", riqueza = "Só riqueza", abundancia = "Só abundância")),
  descritiva_pontos = list(nome = "Riqueza e abundância por ponto", padrao = "lado_a_lado", formas = list(
    lado_a_lado = "Um gráfico: colunas lado a lado, eixo de espécies à esquerda e de indivíduos à direita", duas = "Dois painéis, um para cada")),
  forma_registro = list(nome = "Forma de registro", padrao = "colunas", formas = list(colunas = "Colunas com número e %", barras = "Barras deitadas com número e %")),
  curva_S = list(nome = "Curva: riqueza observada", padrao = "limpa", formas = list(limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linhas pontilhadas", linha = "Linha com valores e IC 95%", barras = "Barras com valores")),
  curva_chao = list(nome = "Curva: Chao 2", padrao = "limpa", formas = list(limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linhas pontilhadas", linha = "Linha com valores e IC 95%", barras = "Barras com valores")),
  curva_jack1 = list(nome = "Curva: Jackknife 1", padrao = "limpa", formas = list(limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linhas pontilhadas", linha = "Linha com valores e IC 95%", barras = "Barras com valores")),
  curva_jack2 = list(nome = "Curva: Jackknife 2", padrao = "limpa", formas = list(limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linhas pontilhadas", linha = "Linha com valores e IC 95%", barras = "Barras com valores")),
  curva_boot = list(nome = "Curva: Bootstrap", padrao = "limpa", formas = list(limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linhas pontilhadas", linha = "Linha com valores e IC 95%", barras = "Barras com valores")),
  similaridade_pares = list(nome = "Similaridade entre pares de pontos", padrao = "pares", formas = list(
    pares = "Pares ordenados: Jaccard e Bray-Curtis", triangulo = "Matriz em triângulo com valores")),
  beta = list(nome = "Diversidade beta (Baselga)", padrao = "barras", formas = list(barras = "Substituição e aninhamento", pares = "Substituição entre pares de pontos")),
  shannon_ic = list(nome = "Shannon por ponto com IC", padrao = "pontos", formas = list(pontos = "Pontos com IC 95% (Hutcheson)"))
))
NOMES_EST <- c(S = "Riqueza observada", chao = "Chao 2", jack1 = "Jackknife 1", jack2 = "Jackknife 2", boot = "Bootstrap")

desenhar_extra <- function(chave, forma, G, O, e, arq, w, h, dpi, gg, pal, k) {
  fmt1 <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
  if (chave %in% names(catalogo_camp())) return(desenhar_camp(chave, forma, G, e, gg, pal, k, w, h))
  if (chave %in% names(catalogo_novas())) return(desenhar_novas(chave, forma, G, e, gg, pal, k, w, h))
  if (chave %in% names(catalogo_fito())) return(desenhar_fito(chave, forma, G, e, gg, pal, k, w, h))
  if (grepl("^est([0-9]|c)_", chave)) return(desenhar_est(chave, forma, G, e, gg, pal, k, w, h))
  gr <- G$grupo; D <- G$descritiva; tam <- e$fonte / 3.1
  rotx <- ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  if (chave == "familias" && !is.null(D)) {
    tf <- D$familias; tf$familia <- factor(tf$familia, levels = rev(tf$familia))
    if (forma == "lado_a_lado") {
      d <- rbind(data.frame(familia = tf$familia, medida = "Riqueza", v = tf$riqueza, p = tf$riqueza_pct),
                 data.frame(familia = tf$familia, medida = "Abundância", v = tf$abundancia, p = tf$abundancia_pct))
      d$medida <- factor(d$medida, levels = unique(d$medida)); d$familia <- factor(d$familia, levels = rev(levels(tf$familia)))
      colunas <- if (tem_hachura(e)) list(ggpattern::geom_col_pattern(ggplot2::aes(pattern = medida), position = ggplot2::position_dodge(.8), width = .75,
            colour = "#2B2620", linewidth = .25, pattern_fill = pal[2], pattern_colour = NA, pattern_angle = 45, pattern_density = .45, pattern_spacing = .018),
          ggpattern::scale_pattern_manual(values = c("none", "stripe"), name = NULL))
        else list(ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75))
      cor_ab <- if (tem_hachura(e)) grDevices::adjustcolor(pal[2], .18) else pal[2]
      # barras deitadas (19/11/2026): nomes das famílias na horizontal, a maior em cima
      d$familia <- factor(d$familia, levels = rev(levels(d$familia))); d$medida <- factor(d$medida, levels = rev(levels(d$medida)))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(familia, p, fill = medida)) + colunas +
        ggplot2::geom_text(ggplot2::aes(label = paste0(v, " (", fmt1(100 * p, 0), "%)")), position = ggplot2::position_dodge(.8), hjust = -.08,
                           family = e$familia, size = tam * .85, colour = k$tinta) +
        ggplot2::scale_fill_manual(values = c(Riqueza = pal[1], "Abundância" = cor_ab), breaks = c("Riqueza", "Abundância"), name = NULL) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = ggplot2::expansion(mult = c(0, .18))) + ggplot2::coord_flip() +
        titulos_gg(e, chave, "Riqueza e abundância por família", NULL, "Porcentagem do total",
                   sprintf("Altura = %% do total de espécies (%d) e de indivíduos (%d); o número em cima é a contagem.", sum(tf$riqueza), sum(tf$abundancia))) +
        tema_gg(e) + leg_dentro(k) + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.major.x = if (isTRUE(e$grade)) ggplot2::element_line(colour = k$grade, linewidth = .4) else ggplot2::element_blank(),
                                    axis.text.y = ggplot2::element_text(colour = k$tinta)), w, max(3.2, .55 * nrow(tf) + 1.6)))
    }
    if (forma == "colunas") {
      d <- rbind(data.frame(familia = tf$familia, medida = "Riqueza", v = tf$riqueza, p = tf$riqueza_pct),
                 data.frame(familia = tf$familia, medida = "Abundância", v = tf$abundancia, p = tf$abundancia_pct))
      d$medida <- factor(d$medida, levels = unique(d$medida)); d$familia <- factor(d$familia, levels = rev(levels(tf$familia)))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(familia, v, fill = medida)) + ggplot2::geom_col(width = .62, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(v, " (", fmt1(100 * p, 0), "%)")), vjust = -.5, family = e$familia, size = tam * .95, colour = k$tinta) +
        ggplot2::facet_wrap(~medida, ncol = 1, scales = "free_y") + ggplot2::scale_fill_manual(values = pal[1:2]) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .2))) +
        titulos_gg(e, chave, "Riqueza e abundância por família", NULL, NULL) + tema_gg(e) + rotx, w * 1.1, h * 1.45))
    }
    if (forma == "duas") {
      d <- rbind(data.frame(familia = tf$familia, medida = "Riqueza", v = tf$riqueza, p = tf$riqueza_pct),
                 data.frame(familia = tf$familia, medida = "Abundância", v = tf$abundancia, p = tf$abundancia_pct))
      d$medida <- factor(d$medida, levels = c("Riqueza", "Abundância"))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(v, familia, fill = medida)) + ggplot2::geom_col(width = .66, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(v, " (", fmt1(100 * p, 0), "%)")), hjust = -.1, family = e$familia, size = tam, colour = k$tinta) +
        ggplot2::facet_wrap(~medida, scales = "free_x") + ggplot2::scale_fill_manual(values = pal[1:2]) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .35))) +
        titulos_gg(e, chave, paste("Riqueza e abundância por família:", gr), NULL, NULL) + tema_gg(e)))
    }
    col <- if (forma == "riqueza") "riqueza" else "abundancia"; cp <- paste0(col, "_pct")
    tf$familia <- factor(tf$familia, levels = rev(levels(tf$familia)))
    return(gg(ggplot2::ggplot(tf, ggplot2::aes(familia, .data[[col]])) + ggplot2::geom_col(fill = pal[if (col == "riqueza") 1 else 2], width = .62) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data[[col]], " (", fmt1(100 * .data[[cp]], 0), "%)")), vjust = -.5, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
      titulos_gg(e, chave, paste(if (col == "riqueza") "Riqueza" else "Abundância", "por família"), NULL, if (col == "riqueza") "Número de espécies" else "Número de indivíduos") + tema_gg(e) + rotx))
  }
  if (chave == "descritiva_pontos" && !is.null(D)) {
    tp <- D$pontos; tp <- tp[(tp$riqueza %||% 0) > 0 | (tp$abundancia %||% 0) > 0, , drop = FALSE]   # ponto sem registro não aparece
    if (forma == "familias") {
      le <- D$especies; pts <- tp$ponto
      d <- do.call(rbind, lapply(pts, function(p) { z <- le[le[[p]] > 0, ]; if (!nrow(z)) return(NULL); as.data.frame(table(familia = z$familia), responseName = "n") |> transform(ponto = p) }))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, n, fill = familia)) + ggplot2::geom_col(width = .66, colour = k$fundo, linewidth = .4) +
        ggplot2::scale_fill_manual(values = rep(pal, 3)) + titulos_gg(e, chave, paste("Espécies por família em cada ponto:", gr), NULL, "Número de espécies") +
        ggplot2::labs(fill = NULL) + tema_gg(e)))
    }
    d <- rbind(data.frame(ponto = tp$ponto, medida = "Riqueza", v = tp$riqueza), data.frame(ponto = tp$ponto, medida = "Abundância", v = tp$abundancia))
    d$medida <- factor(d$medida, levels = c("Riqueza", "Abundância"))
    if (forma == "lado_a_lado") {
      # 19/11/2026: colunas lado a lado em cada ponto, cores diferentes, sem título (só a legenda).
      # Riqueza e abundância têm escalas muito diferentes: a abundância é desenhada na escala da riqueza (fator f)
      # e lida no eixo da direita, na cor dela; o valor real vai escrito em cima de cada coluna.
      f <- max(tp$abundancia, 1) / max(tp$riqueza, 1)
      d$y <- ifelse(d$medida == "Abundância", d$v / f, d$v)
      cr <- c(Riqueza = pal[1], "Abundância" = pal[2])
      return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, y, fill = medida)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(.78), width = .72) +
        ggplot2::geom_text(ggplot2::aes(label = v, colour = medida), position = ggplot2::position_dodge(.78), vjust = -.45, family = e$familia, size = tam, fontface = "bold", show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = cr, name = NULL) + ggplot2::scale_colour_manual(values = cr, guide = "none") +
        ggplot2::scale_y_continuous(name = rot(e, chave, "y", "Nº de espécies"), expand = ggplot2::expansion(mult = c(0, .12)),
                                    breaks = function(l) unique(floor(pretty(l))),
                                    sec.axis = ggplot2::sec_axis(~ . * f, name = "Nº de indivíduos")) +
        ggplot2::labs(title = if (nzchar(e$titulos[[chave]][["titulo"]] %||% "")) e$titulos[[chave]][["titulo"]] else NULL, subtitle = NULL,
                      x = rot(e, chave, "x", "Pontos amostrais"), caption = nota_fig(e, chave, NULL)) +
        tema_gg(e) + ggplot2::theme(axis.title.y.left = ggplot2::element_text(colour = pal[1]), axis.text.y.left = ggplot2::element_text(colour = pal[1]),
                                    axis.title.y.right = ggplot2::element_text(colour = pal[2]), axis.text.y.right = ggplot2::element_text(colour = pal[2]),
                                    legend.position = "top", legend.justification = "left")))
    }
    eixo <- c(Riqueza = "Nº de espécies", "Abundância" = "Nº de indivíduos")
    return(gg(ggplot2::ggplot(d, ggplot2::aes(ponto, v, fill = medida)) + ggplot2::geom_col(width = .62) +
      ggplot2::geom_text(ggplot2::aes(label = v), vjust = -.5, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::facet_wrap(~medida, scales = "free_y", strip.position = "left", labeller = ggplot2::as_labeller(eixo)) +
      ggplot2::scale_fill_manual(values = c(Riqueza = pal[1], "Abundância" = pal[2]), name = NULL) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
      titulos_gg(e, chave, paste("Riqueza e abundância por ponto:", gr), "Pontos amostrais", NULL) + tema_gg(e) +
      ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 90, hjust = .5, colour = k$tinta2, size = ggplot2::rel(.95), family = e$familia),
                     strip.background = ggplot2::element_blank(), panel.spacing = grid::unit(1.6, "lines"))))
  }
  if (chave == "forma_registro" && !is.null(D) && nrow(D$forma_registro)) {
    tr <- D$forma_registro; tr$rot <- paste0(tr$descricao, " (", tr$forma, ")")
    if (forma == "colunas") return(gg(ggplot2::ggplot(tr, ggplot2::aes(stats::reorder(rot, -individuos), individuos)) + ggplot2::geom_col(fill = pal[3], width = .55) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(individuos, " (", fmt1(100 * pct, 0), "%)")), vjust = -.5, family = e$familia, size = tam * 1.05, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .14))) + titulos_gg(e, chave, "Indivíduos por forma de registro", NULL, "Número de indivíduos") + tema_gg(e), w * .85, h * .9))
    return(gg(ggplot2::ggplot(tr, ggplot2::aes(stats::reorder(rot, individuos), individuos)) + ggplot2::geom_col(fill = pal[3], width = .6) + ggplot2::coord_flip() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(individuos, " (", fmt1(100 * pct, 0), "%)")), hjust = -.1, family = e$familia, size = tam, colour = k$tinta) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .3))) + titulos_gg(e, chave, paste("Indivíduos por forma de registro:", gr), NULL, "Indivíduos") + tema_gg(e), w, h * .7))
  }
  if (chave == "categorias" && !is.null(D) && length(D$categorias)) {
    d <- do.call(rbind, D$categorias)
    return(gg(ggplot2::ggplot(d, ggplot2::aes(especies_pct, stats::reorder(classe, especies_pct))) + ggplot2::geom_col(fill = pal[1], width = .62) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(especies_n, " (", fmt1(100 * especies_pct, 0), "%)")), hjust = -.1, family = e$familia, size = tam * .9, colour = k$tinta) +
      ggplot2::facet_wrap(~categoria, scales = "free_y", ncol = 2) + ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.25), breaks = c(0, .5, 1)) +
      titulos_gg(e, chave, paste("Atributos das espécies:", gr), "Porcentagem das espécies", NULL) + tema_gg(e), w * 1.15, h * 1.5))
  }
  if (grepl("^curva_", chave)) {
    est <- sub("^curva_", "", chave); pool <- G$acumulacao$poolaccum
    if (est == "S") z <- data.frame(pontos = G$acumulacao$curva$pontos, media = G$acumulacao$curva$riqueza, li = G$acumulacao$curva$li, ls = G$acumulacao$curva$ls)
    else { if (is.null(pool)) return(invisible()); z <- pool[pool$estimador == est, ] }
    cor <- c(S = k$tinta, chao = pal[1], jack1 = pal[2], jack2 = pal[3], boot = pal[4])[[est]]
    obs <- tail(G$acumulacao$curva$riqueza, 1)
    tt <- paste0(NOMES_EST[[est]], " por número de pontos: ", gr)
    if (forma == "limpa") return(curvas_limpas(chave, list(if (est == "S") curva_obs_inext(G) else curva_estimador(G, est)), NOMES_EST[[est]], cor, e, gg, k, w, h))
    if (forma == "inext") return(curvas_estilo_inext(chave, list(if (est == "S") curva_obs_inext(G) else curva_estimador(G, est)), NOMES_EST[[est]], cor, G, e, gg, pal, k, w, h))
    if (forma == "barras") return(gg(ggplot2::ggplot(z, ggplot2::aes(factor(pontos), media)) + ggplot2::geom_col(fill = cor, width = .6) +
      { if (isTRUE(e$faixa)) ggplot2::geom_errorbar(ggplot2::aes(ymin = li, ymax = ls), width = .15, colour = k$tinta2) } +
      ggplot2::geom_text(ggplot2::aes(y = if (isTRUE(e$faixa)) ls else media, label = fmt1(media)), vjust = -.6, family = e$familia, size = tam * 1.15, colour = k$tinta, fontface = "bold") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
      titulos_gg(e, chave, tt, "Número de pontos amostrais", "Número de espécies", "Barras de erro = IC 95%.") + tema_gg(e)))
    return(gg(ggplot2::ggplot(z, ggplot2::aes(pontos, media)) +
      faixa_ic(e, z, cor, NULL, "pontos") +
      { if (est != "S") ggplot2::geom_hline(yintercept = obs, linetype = 3, colour = k$tinta2) } +
      { if (est != "S") ggplot2::annotate("text", x = min(z$pontos), y = obs, label = paste("observada =", obs), vjust = -.5, hjust = 0, family = e$familia, size = tam * .9, colour = k$tinta2) } +
      ggplot2::geom_line(colour = cor, linewidth = e$linha * 1.2) + ggplot2::geom_point(colour = cor, size = e$ponto * 1.3) +
      ggplot2::geom_label(ggplot2::aes(label = fmt1(media)), vjust = -.55, family = e$familia, size = tam * 1.1, colour = cor, fill = k$fundo, linewidth = 0, fontface = "bold") +
      ggplot2::scale_x_continuous(breaks = z$pontos) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.05, .15))) +
      titulos_gg(e, chave, tt, "Número de pontos amostrais", "Número de espécies", if (est == "S") "vegan::specaccum; faixa = IC 95%." else "vegan::poolaccum; faixa = IC 95%; linha pontilhada = riqueza observada.") + tema_gg(e)))
  }
  if (chave == "similaridade_pares" && !is.null(G$similaridade)) {
    pp <- G$similaridade$pares
    if (forma == "triangulo") {
      S <- as.matrix(G$similaridade$jaccard); B <- as.matrix(G$similaridade$bray); r <- G$similaridade$dendro_jaccard$rotulos
      d <- do.call(rbind, lapply(seq_along(r), function(i) do.call(rbind, lapply(seq_along(r), function(j) if (i == j) NULL else
        data.frame(a = r[i], b = r[j], v = if (i > j) S[i, j] else B[i, j], idx = if (i > j) "Jaccard (abaixo)" else "Bray-Curtis (acima)")))))
      d$a <- factor(d$a, levels = r); d$b <- factor(d$b, levels = rev(r))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(a, b, fill = v)) + ggplot2::geom_tile(colour = k$fundo, linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = fmt1(v, 2)), family = e$familia, size = tam * 1.1, colour = ifelse(d$v > .5, "white", k$tinta)) +
        ggplot2::scale_fill_gradient(low = "#f1eee6", high = pal[1], limits = c(0, 1), name = "Similaridade") + ggplot2::coord_equal() +
        titulos_gg(e, chave, paste("Similaridade entre pontos:", gr), NULL, NULL, "Abaixo da diagonal: Jaccard (presença). Acima: Bray-Curtis (abundância). 1 = iguais; 0 = nada em comum.") + tema_gg(e), h * 1.35, h))
    }
    d <- rbind(data.frame(par = pp$par, indice = "Jaccard (lista de espécies)", v = pp$jaccard, n = pp$compartilhadas),
               data.frame(par = pp$par, indice = "Bray-Curtis (quantidades)", v = pp$bray_curtis, n = pp$compartilhadas))
    d$par <- factor(d$par, levels = rev(pp$par)); d$indice <- factor(d$indice, levels = c("Jaccard (lista de espécies)", "Bray-Curtis (quantidades)"))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(v, par, fill = indice)) + ggplot2::geom_col(position = ggplot2::position_dodge(.7), width = .65) +
      ggplot2::geom_text(ggplot2::aes(label = fmt1(v, 2)), position = ggplot2::position_dodge(.7), hjust = -.15, family = e$familia, size = tam * .95, colour = k$tinta) +
      ggplot2::geom_vline(xintercept = .5, linetype = 3, colour = k$tinta2) +
      ggplot2::scale_fill_manual(values = pal[1:2], name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) + ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = seq(0, 1, .25), labels = function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")) +
      titulos_gg(e, chave, paste("Similaridade entre pares de pontos:", gr), "Similaridade (0 = nada em comum; 1 = iguais)", NULL,
                 "Pares do mais parecido para o menos parecido. Linha pontilhada = metade das espécies em comum.") + tema_gg(e), w, h * 1.3))
  }
  if (chave == "beta" && !is.null(G$beta)) {
    b <- G$beta
    if (forma == "pares") {
      Ms <- as.matrix(b$par_turnover); r <- rownames(Ms)
      d <- do.call(rbind, lapply(seq_along(r), function(i) do.call(rbind, lapply(seq_along(r), function(j) if (i <= j) NULL else data.frame(a = r[i], b = r[j], v = Ms[i, j])))))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(b, a, fill = v)) + ggplot2::geom_tile(colour = k$fundo, linewidth = 1.2) +
        ggplot2::geom_text(ggplot2::aes(label = fmt1(v, 2)), family = e$familia, size = tam * 1.1, colour = ifelse(d$v > .5, "white", k$tinta)) +
        ggplot2::scale_fill_gradient(low = "#f1eee6", high = pal[2], limits = c(0, 1), name = "Substituição") + ggplot2::coord_equal() +
        titulos_gg(e, chave, paste("Substituição de espécies entre pontos:", gr), NULL, NULL) + tema_gg(e), h * 1.3, h))
    }
    d <- data.frame(parte = factor(c("Substituição (turnover)", "Aninhamento"), levels = c("Aninhamento", "Substituição (turnover)")), v = c(b$turnover, b$aninhamento))
    return(gg(ggplot2::ggplot(d, ggplot2::aes(x = "Diferença total entre os pontos (Sørensen)", y = v, fill = parte)) + ggplot2::geom_col(width = .45, colour = k$fundo) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(parte, "\n", fmt1(v, 2))), position = ggplot2::position_stack(vjust = .5), family = e$familia, size = tam, colour = "white") +
      ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = pal[c(4, 2)], guide = "none") + ggplot2::scale_y_continuous(limits = c(0, 1)) +
      titulos_gg(e, chave, paste("Partição da diversidade beta (Baselga):", gr), NULL, "Dissimilaridade de Sørensen",
                 sprintf("Gama = %d espécies; alfa médio = %s; beta de Whittaker = %s.", b$gama, fmt1(b$alfa_media), fmt1(b$whittaker, 2))) + tema_gg(e), w, h * .6))
  }
  if (chave == "shannon_ic" && !is.null(G$hutcheson)) {
    ic <- G$hutcheson$ic
    return(gg(ggplot2::ggplot(ic, ggplot2::aes(ponto, H)) + ggplot2::geom_errorbar(ggplot2::aes(ymin = li, ymax = ls), width = .15, colour = k$tinta2, linewidth = e$linha) +
      ggplot2::geom_point(size = e$ponto * 1.6, colour = pal[1]) + ggplot2::geom_text(ggplot2::aes(label = fmt1(H, 2)), nudge_x = .28, family = e$familia, size = tam, colour = k$tinta) +
      titulos_gg(e, chave, paste("Diversidade de Shannon por ponto, com IC 95%:", gr), NULL, "Shannon (H')", "Intervalos pela variância de Hutcheson. Intervalos que não se sobrepõem indicam diferença.") + tema_gg(e)))
  }
  invisible()
}

# ---- dominância com nome, ranque e cor por espécie ----
dominancia_nomes <- function(G, e, chave, gg, pal, k) {
  ra <- G$abundancia$tabela; fam <- G$descritiva$especies$familia[match(ra$especie, G$descritiva$especies$especie)]
  ra$familia <- ifelse(is.na(fam), "Outra", fam)
  ra$rot <- paste0(ra$posicao, ". ", sub("^(\\w)\\w+ ", "\\1. ", ra$especie))
  fams <- unique(ra$familia)
  gg(ggplot2::ggplot(ra, ggplot2::aes(posicao, relativa)) + ggplot2::geom_line(colour = k$tinta2, linewidth = e$linha * .7) +
    ggplot2::geom_point(ggplot2::aes(colour = familia), size = e$ponto * 1.6) +
    ggplot2::geom_text(ggplot2::aes(label = rot, colour = familia), angle = 50, hjust = -.12, vjust = .2, family = e$familia, size = e$fonte / 3.6, fontface = "italic", show.legend = FALSE) +
    ggplot2::scale_y_log10(labels = function(x) paste0(format(100 * x, decimal.mark = ",", drop0trailing = TRUE), "%"), expand = ggplot2::expansion(mult = c(.05, .55))) +
    ggplot2::scale_x_continuous(breaks = ra$posicao, expand = ggplot2::expansion(add = c(.6, 2.5))) +
    ggplot2::scale_colour_manual(values = setNames(rep(pal, length.out = length(fams)), fams), name = "Família") +
    titulos_gg(e, chave, paste("Curva de dominância:", G$grupo), "Posição no ranque (1 = mais abundante)", "Abundância relativa (log)") + tema_gg(e),
    e$largura * 1.25, e$altura * 1.35)
}
