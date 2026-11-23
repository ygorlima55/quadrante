# Quadrante: figuras feitas no R (vegan, iNEXT, ggplot2 e gráficos base do R), tabelas em Excel e distâncias.
# Cada figura tem várias "formas" (catálogo abaixo). A forma escolhida, o tema, as cores, as letras e os títulos
# vêm do estilo.json da pasta do levantamento, editado no painel.

estilo_padrao <- list(
  # Direção A "Sálvia" (17/11/2026): paleta validada para daltonismo sobre #f7f7f5
  # (escuro: #4fa274 #e05a3b #5385d4 #bb8708 #ad5fa4 #6f9e24 #4596e0 #c4692f sobre #1a1d1b).
  # A Direção C "Síntese" continua como tema_r = "sintese" (cópia em versoes/direcao-c-sintese-2026-11-17).
  paleta = c("#3f8c62", "#d0482a", "#2a5cab", "#a87700", "#9a3f8f", "#5c8a00", "#3d8fdc", "#ad4f19"),
  # 18/11/2026: tudo em Times New Roman, legenda em cima e na horizontal, sem notas de rodapé
  # explicativas (mostrar_nota), JPEG de alta resolução ao lado de cada PNG (dpi_jpeg).
  tema_r = "salvia", fonte = 11, familia = "Times New Roman", familia_titulo = "Times New Roman",
  tema = "claro", faixa = TRUE, grade = FALSE, linha = 1, ponto = 2.5,
  legenda = "top", largura = 7, altura = 4.5, dpi = 300, dpi_jpeg = 600, mostrar_nota = FALSE, mostrar_titulo = TRUE, mostrar_grupo = FALSE, cores_fig = list(), ajustes = list(),
  titulos = list(), formas = list()
)

catalogo_formas <- function() list(
  acumulacao = list(nome = "Curva de acumulação", padrao = "limpa", formas = list(
    limpa = "Limpa: só a curva, a faixa e a legenda no canto", inext = "Estilo iNEXT: curva com faixa, amostra real e linha pontilhada", vegan = "vegan: plot(specaccum) com caixas", faixa = "ggplot: linha e faixa de 95%", estimadores = "Observada com Chao 2, Jackknife e Bootstrap")),
  estimadores = list(nome = "Estimadores de riqueza", padrao = "curvas", formas = list(
    curvas = "Curvas dos estimadores por ponto (poolaccum)", lattice = "vegan: plot(poolaccum), um painel por estimador",
    barras = "Barras com erro padrão", pontos = "Pontos com erro padrão")),
  inext_individuos = list(nome = "iNEXT por indivíduos", padrao = "limpa", formas = list(
    limpa = "Limpa: só a curva, a faixa e a legenda no canto", clara = "Riqueza com observado e estimador, legenda explicada", hill = "Números de Hill (q 0, 1 e 2) com legenda explicada", ggiNEXT = "ggiNEXT: rarefação e extrapolação", facetas = "ggiNEXT: um painel por q", cobertura = "ggiNEXT: diversidade pela cobertura", completude = "ggiNEXT: completude da amostra")),
  inext_pontos = list(nome = "iNEXT por pontos", padrao = "limpa", formas = list(
    limpa = "Limpa: só a curva, a faixa e a legenda no canto", clara = "Riqueza com observado e estimadores, legenda explicada", hill = "Números de Hill (q 0, 1 e 2) com legenda explicada", ggiNEXT = "ggiNEXT: rarefação e extrapolação", facetas = "ggiNEXT: um painel por q", cobertura = "ggiNEXT: diversidade pela cobertura", completude = "ggiNEXT: completude da amostra")),
  rarefacao_pontos = list(nome = "Rarefação por ponto", padrao = "rarecurve", formas = list(
    rarecurve = "vegan: rarecurve", ggiNEXT = "ggiNEXT: cada ponto com faixa")),
  indices_pontos = list(nome = "Índices por ponto", padrao = "hill", formas = list(
    hill = "Espécies efetivas por ponto (Hill), com valores", barras = "Um painel por índice, com valores", pontos = "Pontos e hastes", renyi = "Perfil de diversidade (Rényi / Hill)", base = "R base: barplot")),
  similaridade = list(nome = "Mapa de similaridade", padrao = "heatmap", formas = list(
    heatmap = "R base: heatmap() com dendrogramas", ladrilhos = "ggplot: ladrilhos com valores")),
  dendrograma_jaccard = list(nome = "Dendrograma Jaccard", padrao = "base", formas = list(
    base = "R base: plot(hclust) com grupos", horizontal = "Horizontal", triangulo = "Triangular", ggplot = "ggplot")),
  dendrograma_bray = list(nome = "Dendrograma Bray-Curtis", padrao = "base", formas = list(
    base = "R base: plot(hclust) com grupos", horizontal = "Horizontal", triangulo = "Triangular", ggplot = "ggplot")),
  whittaker = list(nome = "Curva de dominância", padrao = "nomes", formas = list(
    nomes = "Ranque com o nome de cada espécie, cor por família", radfit = "vegan: radfit, modelos de abundância", linha = "ggplot: posição × abundância (log)", barras = "Barras por espécie"))
)
catalogo_todo <- function() c(catalogo_formas(), catalogo_extra())

ler_estilo <- function(caminhos) {
  e <- estilo_padrao
  for (f in caminhos) if (file.exists(f)) {
    j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(x) NULL)
    if (!is.null(j)) for (k in names(j)) if (!is.null(j[[k]])) e[[k]] <- j[[k]]
    break
  }
  e$paleta <- unlist(e$paleta)
  if (is.null(e$titulos) || !is.list(e$titulos)) e$titulos <- list()
  if (is.null(e$formas) || !is.list(e$formas)) e$formas <- list()
  if (is.null(e$cores_fig) || !is.list(e$cores_fig)) e$cores_fig <- list()
  if (is.null(e$ajustes) || !is.list(e$ajustes)) e$ajustes <- list()
  e
}

registrar_fontes <- function(e) {
  if (.Platform$OS.type != "windows") return(invisible())
  fs <- unique(c(e$familia, e$familia_titulo))
  do.call(grDevices::windowsFonts, setNames(lapply(fs, grDevices::windowsFont), fs))
}

rot <- function(e, chave, campo, padrao) {
  t <- e$titulos[[chave]][[campo]]
  if (is.null(t) || !nzchar(t)) padrao else t
}
cores_tema <- function(e) {
  escuro <- identical(e$tema, "escuro")
  if (identical(e$tema_r %||% "", "salvia"))   # Sálvia: cinza quente claro ou grafite
    return(if (escuro) list(tinta = "#eceeea", tinta2 = "#a3aba5", fundo = "#1a1d1b", grade = "#2c312e")
           else list(tinta = "#1d1f1e", tinta2 = "#5c615c", fundo = "#ffffff", grade = "#e4e4e0"))
  if (!escuro && identical(e$tema_r %||% "", "sintese"))   # papel creme, nanquim, grade sépia
    return(list(tinta = "#2B2620", tinta2 = "#55493B", fundo = "#F7F1E3", grade = "#E3D7BF"))
  list(tinta = if (escuro) "#ecebe3" else "#24231f", tinta2 = if (escuro) "#b8bab3" else "#5d5a52",
       fundo = if (escuro) "#23282b" else "#ffffff", grade = if (escuro) "#3a4246" else "#e7e4dc")
}

tema_gg <- function(e) {
  k <- cores_tema(e); bs <- e$fonte; fam <- e$familia
  base <- switch(e$tema_r %||% "prancha",
    ggplot = ggplot2::theme_grey(bs, fam), bw = ggplot2::theme_bw(bs, fam), classic = ggplot2::theme_classic(bs, fam),
    minimal = ggplot2::theme_minimal(bs, fam), ggplot2::theme_minimal(bs, fam))
  horiz <- e$legenda %in% c("top", "bottom")
  t <- base + ggplot2::theme(legend.position = e$legenda, plot.title.position = "plot",
    legend.direction = if (horiz) "horizontal" else "vertical", legend.box = if (horiz) "horizontal" else "vertical",
    legend.justification = if (horiz) "left" else "center",
    legend.margin = ggplot2::margin(0, 0, 0, 0), legend.box.spacing = grid::unit(4, "pt"),
    legend.key.size = grid::unit(.9, "lines"), legend.spacing.x = grid::unit(4, "pt"),
    plot.title = ggplot2::element_text(family = e$familia_titulo, face = "bold", size = ggplot2::rel(1.15)),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(.8)))
  if (identical(e$tema_r %||% "", "salvia")) {
    salvia_esc <- if (identical(e$tema, "escuro")) "#8fb59e" else "#4c7a5f"
    t <- t + ggplot2::theme(text = ggplot2::element_text(colour = k$tinta), axis.text = ggplot2::element_text(colour = k$tinta2, size = ggplot2::rel(.92)),
      axis.title = ggplot2::element_text(colour = k$tinta2, size = ggplot2::rel(.95)),
      plot.title = ggplot2::element_text(family = e$familia_titulo, face = "plain", size = ggplot2::rel(1.3), colour = k$tinta, margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(colour = salvia_esc, size = ggplot2::rel(.95), margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(.8), colour = k$tinta2, hjust = 0),
      plot.caption.position = "plot",
      panel.grid.major = if (isTRUE(e$grade)) ggplot2::element_line(colour = k$grade, linewidth = .4) else ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = k$tinta2, linewidth = .4), axis.ticks = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA), panel.background = ggplot2::element_rect(fill = k$fundo, colour = NA),
      plot.margin = ggplot2::margin(16, 18, 12, 14),
      strip.text = ggplot2::element_text(family = e$familia_titulo, colour = k$tinta, size = ggplot2::rel(1), hjust = 0),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = k$fundo, colour = NA), legend.key = ggplot2::element_rect(fill = k$fundo, colour = NA),
      legend.text = ggplot2::element_text(colour = k$tinta, size = ggplot2::rel(.9)), legend.title = ggplot2::element_text(colour = k$tinta2, size = ggplot2::rel(.85)))
  } else if (identical(e$tema_r %||% "", "sintese") && !identical(e$tema, "escuro")) {
    t <- t + ggplot2::theme(text = ggplot2::element_text(colour = k$tinta), axis.text = ggplot2::element_text(colour = k$tinta2),
      plot.title = ggplot2::element_text(family = e$familia_titulo, face = "plain", size = ggplot2::rel(1.3), colour = k$tinta),
      plot.subtitle = ggplot2::element_text(family = e$familia_titulo, face = "italic", colour = "#00747B"),
      plot.caption = ggplot2::element_text(family = e$familia_titulo, face = "italic", size = ggplot2::rel(.82), colour = k$tinta2),
      panel.grid.major = if (isTRUE(e$grade)) ggplot2::element_line(colour = k$grade, linewidth = .35) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = k$tinta, linewidth = .45), axis.ticks = ggplot2::element_line(colour = k$tinta, linewidth = .4),
      axis.ticks.length = grid::unit(3, "pt"),
      plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA), panel.background = ggplot2::element_rect(fill = k$fundo, colour = NA),
      strip.text = ggplot2::element_text(family = e$familia_titulo, face = "italic", colour = k$tinta, size = ggplot2::rel(1.05)),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = k$fundo, colour = NA), legend.key = ggplot2::element_rect(fill = k$fundo, colour = NA))
  } else if (identical(e$tema_r %||% "prancha", "prancha") || identical(e$tema, "escuro")) {
    t <- t + ggplot2::theme(text = ggplot2::element_text(colour = k$tinta), axis.text = ggplot2::element_text(colour = k$tinta2),
      panel.grid.major = if (isTRUE(e$grade)) ggplot2::element_line(colour = k$grade, linewidth = .4) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(), axis.line.x = ggplot2::element_line(colour = k$tinta2, linewidth = .4),
      plot.background = ggplot2::element_rect(fill = k$fundo, colour = NA), panel.background = ggplot2::element_rect(fill = k$fundo, colour = NA),
      strip.text = ggplot2::element_text(face = "bold", colour = k$tinta), legend.background = ggplot2::element_rect(fill = k$fundo, colour = NA))
  } else if (!isTRUE(e$grade)) t <- t + ggplot2::theme(panel.grid = ggplot2::element_blank())
  t
}
tira_grupo <- function(e, titulo) if (is.null(titulo) || is.null(e$grupo_atual)) titulo else sub(paste0(":\\s*", e$grupo_atual, "$"), "", titulo)
# títulos padrão de cada figura (o painel mostra no editor de títulos)
.TIT <- new.env()
titulos_padrao <- function() { out <- list(); for (k in ls(.TIT)) { p <- strsplit(k, "\r", fixed = TRUE)[[1]]; out[[p[1]]][[p[2]]] <- get(k, envir = .TIT) }; out }
titulos_gg <- function(e, chave, titulo, x, y, nota = NULL) {
  if (!is.null(e$grupo_atual)) assign(paste(e$grupo_atual, chave, sep = "\r"), Filter(Negate(is.null), list(titulo = tira_grupo(e, titulo), x = x, y = y, nota = if (nzchar(nota %||% "")) nota)), envir = .TIT)
  ggplot2::labs(title = if (isTRUE(e$mostrar_titulo)) rot(e, chave, "titulo", tira_grupo(e, titulo)) else NULL,
                subtitle = if (isTRUE(e$mostrar_titulo) && !isFALSE(e$mostrar_grupo) && !is.null(e$grupo_atual)) e$grupo_atual else NULL,
                x = rot(e, chave, "x", x), y = rot(e, chave, "y", y), caption = nota_fig(e, chave, nota))
}
# nota de rodapé: só a escrita pela pessoa no editor de títulos, ou a padrão quando mostrar_nota = TRUE
nota_fig <- function(e, chave, padrao) {
  t <- e$titulos[[chave]][["nota"]]
  if (!is.null(t) && nzchar(t)) return(t)
  if (isTRUE(e$mostrar_nota)) padrao %||% "" else NULL
}

# ---- dispositivos ----
abrir_png <- function(arq, e, w, h, dpi, jpeg = FALSE) {
  k <- cores_tema(e); tp <- if (.Platform$OS.type == "windows") "windows" else "cairo"
  if (jpeg) grDevices::jpeg(arq, width = w, height = h, units = "in", res = dpi, quality = 100, type = tp, bg = k$fundo)
  else grDevices::png(arq, width = w, height = h, units = "in", res = dpi, type = tp, bg = k$fundo)
  graphics::par(family = e$familia, col.axis = k$tinta2, col.lab = k$tinta, col.main = k$tinta, fg = k$tinta2, mar = c(5.6, 4.5, if (isTRUE(e$mostrar_titulo)) 3.2 else 1.2, 1.2),
                cex = e$fonte / 11, las = 1, lwd = e$linha)
}
titulo_base <- function(e, chave, padrao) if (isTRUE(e$mostrar_titulo)) graphics::title(main = rot(e, chave, "titulo", tira_grupo(e, padrao)), family = e$familia_titulo, font.main = 2, cex.main = 1.1)
nota_base <- function(e, chave, padrao) { n <- nota_fig(e, chave, padrao) %||% ""; if (nzchar(n)) graphics::mtext(n, side = 1, line = 4.4, adj = 1, cex = .75, col = cores_tema(e)$tinta2) }

recriar_hclust <- function(d) structure(list(merge = matrix(unlist(d$merge), ncol = 2), height = unlist(d$altura), order = unlist(d$ordem),
  labels = unlist(d$rotulos), method = "average", call = quote(hclust(d, "average")), dist.method = "dissimilaridade"), class = "hclust")

# nomes e cores da legenda de uma figura (o painel mostra como quadradinhos clicáveis para trocar a cor);
# indice = posição da cor na paleta (1ª cor, 2ª cor...), que é o que o painel troca em cores_fig
legenda_cores <- function(p, paleta, destino) {
  info <- tryCatch({
    out <- NULL
    for (a in c("fill", "colour")) {
      g <- suppressWarnings(ggplot2::get_guide_data(p, a))
      if (!is.null(g) && nrow(g) && a %in% names(g)) {
        cor <- toupper(grDevices::rgb(t(grDevices::col2rgb(g[[a]])), maxColorValue = 255))
        out <- data.frame(rotulo = as.character(g$.label), cor = cor, indice = match(cor, toupper(paleta)), stringsAsFactors = FALSE)
        break
      }
    }
    if (is.null(out)) {   # figura sem legenda (uma cor só): pega as cores da primeira camada
      # 21/11/2026: cada cor ganha o nome do que ela pinta (ex.: barras por família = nome da família no eixo)
      b <- suppressWarnings(ggplot2::ggplot_build(p)); ld <- b$data[[1]]; v <- if (!is.null(ld$fill)) ld$fill else ld$colour
      rot <- NULL
      for (sc in list(b$layout$panel_scales_x[[1]], b$layout$panel_scales_y[[1]])) {
        if (is.null(sc) || !sc$is_discrete()) next
        pos <- ld[[if (any(sc$aesthetics == "x")) "x" else "y"]]; lim <- tryCatch(as.character(sc$get_limits()), error = function(x) NULL)
        if (length(lim) && !is.null(pos)) { rot <- lim[round(as.numeric(pos))]; break } }
      ok <- !is.na(v); v <- v[ok]; rot <- if (!is.null(rot)) rot[ok]
      if (length(v)) {
        cor <- toupper(grDevices::rgb(t(grDevices::col2rgb(v)), maxColorValue = 255)); u <- unique(cor)
        nome <- if (!is.null(rot) && !anyNA(rot)) vapply(u, function(cc) paste(unique(rot[cor == cc]), collapse = ", "), "")
                else if (length(u) == 1) "Cor" else paste("Cor", seq_along(u))
        out <- utils::head(data.frame(rotulo = unname(nome), cor = u, indice = match(u, toupper(paleta)), stringsAsFactors = FALSE), 16) }
    }
    out
  }, error = function(x) NULL)
  if (is.null(info) || !nrow(info)) { unlink(destino); return(invisible()) }
  jsonlite::write_json(info, destino, auto_unbox = TRUE)
}

# ---- uma figura, numa forma ----
desenhar <- function(chave, forma, G, O, e, arq, w, h, dpi) {
  # cores próprias desta figura (escolhidas no painel em "Editar títulos"); o que faltar vem da paleta geral
  cf <- unlist(e$cores_fig[[chave]]); cf <- cf[!is.na(cf) & nzchar(cf)]
  if (length(cf)) e$paleta <- c(cf, e$paleta[-seq_along(cf)])
  pal <- e$paleta; k <- cores_tema(e); lw <- e$linha; pt <- e$ponto; gr <- G$grupo
  fmt1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")
  # figura principal (pasta figuras/): também um JPEG de alta resolução (dpi_jpeg, qualidade 100) com o mesmo nome
  tp <- if (.Platform$OS.type == "windows") "windows" else "cairo"
  arq_jpg <- if (basename(dirname(arq)) == "figuras" && (e$dpi_jpeg %||% 0) > 0) sub("\\.png$", ".jpg", arq) else NULL
  gg <- function(p, ww = w, hh = h) {
    # a legenda original vai para o painel (trocar cor, nome ou tirar um item); depois entram os ajustes pedidos
    if (!is.null(arq_jpg)) legenda_cores(p, e$paleta, sub("\\.png$", ".cores.json", arq))
    if (!is.null(arq_jpg)) p <- ajustar_fig(p, e$ajustes[[chave]])
    ggplot2::ggsave(arq, p, width = ww, height = hh, dpi = dpi, bg = k$fundo, device = "png", type = tp)
    if (!is.null(arq_jpg)) ggplot2::ggsave(arq_jpg, p, width = ww, height = hh, dpi = e$dpi_jpeg, bg = k$fundo, device = "jpeg", type = tp, quality = 100)
    invisible(arq)
  }
  base <- function(fun) {
    abrir_png(arq, e, w, h, dpi); r <- tryCatch(fun(), finally = grDevices::dev.off())
    if (!is.null(arq_jpg)) { abrir_png(arq_jpg, e, w, h, e$dpi_jpeg, jpeg = TRUE); tryCatch(fun(), finally = grDevices::dev.off()) }
    invisible(r)
  }
  e$grupo_atual <- gr
  if (chave %in% names(catalogo_extra())) return(desenhar_extra(chave, forma, G, O, e, arq, w, h, dpi, gg, pal, k))
  if (chave %in% c("inext_individuos", "inext_pontos") && forma == "limpa") {
    z <- if (chave == "inext_individuos") G$inext$abundancia else G$inext$incidencia
    if (is.null(z)) return(invisible()); z <- z[z$q == 0, ]
    return(curvas_limpas(chave, list(data.frame(pontos = z$m, media = z$qD, li = z$li, ls = z$ls, real = z$metodo != "Extrapolation")), "Riqueza (q = 0)", pal[1], e, gg, k, w, h,
                         if (chave == "inext_individuos") "Número de indivíduos" else "Número de pontos amostrais"))
  }
  if (chave %in% c("inext_individuos", "inext_pontos") && forma %in% c("clara", "hill")) return(inext_claro(chave, forma, G, e, gg, pal, k, w, h))
  if (chave == "whittaker" && forma == "nomes" && !is.null(G$descritiva)) return(dominancia_nomes(G, e, chave, gg, pal, k))
  nomes_est <- c(S = "Observada", chao = "Chao 2", jack1 = "Jackknife 1", jack2 = "Jackknife 2", boot = "Bootstrap")

  if (chave == "acumulacao" && forma == "limpa" && !is.null(G$acumulacao$curva))
    return(curvas_limpas(chave, list(curva_obs_inext(G)), "Riqueza observada", pal[1], e, gg, k, w, h))
  if (chave == "acumulacao" && forma == "inext" && !is.null(G$acumulacao$curva)) {
    c1 <- G$acumulacao$curva
    return(curvas_estilo_inext(chave, list(curva_obs_inext(G)), "Riqueza observada", pal[1], G, e, gg, pal, k, w, h))
  }
  if (chave == "acumulacao") {
    tt <- paste("Curva de acumulação de espécies:", gr); xl <- rot(e, chave, "x", "Número de pontos amostrais"); yl <- rot(e, chave, "y", "Riqueza acumulada")
    if (forma == "vegan" && !is.null(O$sa)) return(base(function() {
      plot(O$sa, ci.type = "poly", col = pal[1], lwd = 2 * lw, ci.lty = 0, ci.col = grDevices::adjustcolor(pal[1], .18), xlab = xl, ylab = yl, xaxt = "n")
      graphics::axis(1, at = O$sa$sites); graphics::boxplot(O$sa, add = TRUE, pch = "+", col = k$fundo, border = k$tinta2, xaxt = "n", yaxt = "n")
      titulo_base(e, chave, tt); nota_base(e, chave, "vegan::specaccum, 999 ordens aleatórias; caixas = permutações")
    }))
    if (forma == "estimadores" && !is.null(G$acumulacao$poolaccum)) forma <- "curvas_est" else if (forma != "faixa") forma <- "faixa"
    if (forma == "faixa") {
      c1 <- G$acumulacao$curva
      return(gg(ggplot2::ggplot(c1, ggplot2::aes(pontos, riqueza)) +
        faixa_ic(e, c1, pal[1], NULL, "pontos") +
        ggplot2::geom_line(colour = pal[1], linewidth = lw) + ggplot2::geom_point(colour = pal[1], size = pt) + ggplot2::scale_x_continuous(breaks = c1$pontos) +
        titulos_gg(e, chave, tt, xl, yl, "Média de 999 ordens aleatórias dos pontos; faixa = IC 95%.") + tema_gg(e)))
    }
    chave2 <- chave
  }
  if (chave == "estimadores" || (chave == "acumulacao" && forma == "curvas_est")) {
    ch <- if (chave == "acumulacao") "acumulacao" else "estimadores"
    tt <- paste("Riqueza observada e estimada:", gr)
    est <- G$acumulacao$estimadores; pool <- G$acumulacao$poolaccum
    if (forma %in% c("curvas", "curvas_est") && !is.null(pool)) {
      pool$estimador <- factor(nomes_est[pool$estimador], levels = nomes_est)
      cs <- setNames(c(k$tinta, pal[1:4]), nomes_est)
      return(gg(ggplot2::ggplot(pool, ggplot2::aes(pontos, media, colour = estimador, fill = estimador)) +
        faixa_ic(e, pool, cs, "estimador", "pontos") +
        ggplot2::geom_line(linewidth = lw) + ggplot2::geom_point(size = pt) +
        ggplot2::scale_colour_manual(values = cs) + ggplot2::scale_fill_manual(values = cs) + ggplot2::scale_x_continuous(breaks = unique(pool$pontos)) +
        titulos_gg(e, ch, tt, "Número de pontos amostrais", "Número de espécies", "vegan::poolaccum, 199 permutações; faixas = IC 95%.") +
        ggplot2::labs(colour = NULL, fill = NULL) + tema_gg(e)))
    }
    if (forma == "lattice" && !is.null(O$pa)) {
      p <- suppressWarnings(plot(O$pa, display = c("S", "chao", "jack1", "jack2", "boot"), col = pal[1], lwd = lw * 2,
                main = if (isTRUE(e$mostrar_titulo)) rot(e, ch, "titulo", tt) else NULL, xlab = "Número de pontos amostrais", ylab = "Número de espécies"))
      grDevices::png(arq, width = w, height = h, units = "in", res = dpi, type = if (.Platform$OS.type == "windows") "windows" else "cairo", bg = k$fundo)
      print(p); grDevices::dev.off(); return(invisible())
    }
    est$estimador <- factor(est$estimador, levels = est$estimador)
    if (forma == "pontos") return(gg(ggplot2::ggplot(est, ggplot2::aes(estimador, valor)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = valor - ep, ymax = valor + ep), width = .12, colour = k$tinta2, na.rm = TRUE) +
      ggplot2::geom_point(size = pt * 1.6, colour = c(k$tinta2, pal[1:4])[seq_len(nrow(est))]) +
      ggplot2::geom_text(ggplot2::aes(label = fmt1(valor)), nudge_x = .28, family = e$familia, size = e$fonte / 3.2, colour = k$tinta) +
      titulos_gg(e, ch, tt, NULL, "Número de espécies", "Barras = erro padrão.") + tema_gg(e)))
    return(gg(ggplot2::ggplot(est, ggplot2::aes(estimador, valor)) + ggplot2::geom_col(fill = c("#9a968c", pal[1:4])[seq_len(nrow(est))], width = .62) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = valor - ep, ymax = valor + ep), width = .18, colour = k$tinta2, na.rm = TRUE) +
      ggplot2::geom_text(ggplot2::aes(label = fmt1(valor)), vjust = -.7, family = e$familia, size = e$fonte / 3.2, colour = k$tinta) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
      titulos_gg(e, ch, tt, NULL, "Número de espécies", "Barras de erro = erro padrão.") + tema_gg(e)))
  }
  if (chave %in% c("inext_individuos", "inext_pontos")) {
    obj <- if (chave == "inext_individuos") O$inx else O$inc
    if (is.null(obj)) return(invisible())
    tipo <- switch(forma, cobertura = 3, completude = 2, 1)
    tt <- paste(if (chave == "inext_individuos") "Rarefação e extrapolação por indivíduos:" else "Rarefação e extrapolação por pontos:", gr)
    p <- suppressWarnings(iNEXT::ggiNEXT(obj, type = tipo, facet.var = if (forma == "facetas") "Order.q" else "None", color.var = if (tipo == 2) "None" else "Order.q", se = isTRUE(e$faixa)))
    nq <- c("Riqueza (q = 0)", "Espécies comuns: Shannon (q = 1)", "Espécies dominantes: Simpson (q = 2)")
    p <- p + ggplot2::scale_colour_manual(values = pal, labels = nq, name = NULL) + ggplot2::scale_fill_manual(values = pal, labels = nq, name = NULL) +
      ggplot2::scale_shape_discrete(labels = nq, name = NULL) + ggplot2::scale_linetype_manual(values = c(1, 2), labels = c("Rarefação (dentro da amostra real)", "Extrapolação (projeção além da amostra)"), name = NULL) +
      titulos_gg(e, chave, tt, switch(as.character(tipo), "3" = "Cobertura da amostra", if (chave == "inext_individuos") "Número de indivíduos" else "Número de pontos amostrais"),
                 if (tipo == 2) "Cobertura da amostra" else "Diversidade de espécies", "iNEXT; linha cheia: rarefação; tracejada: extrapolação.") + tema_gg(e)
    return(gg(p))
  }
  if (chave == "rarefacao_pontos") {
    tt <- paste("Rarefação de cada ponto amostral:", gr)
    mm <- O$m[rowSums(O$m) > 0, , drop = FALSE]
    if (forma == "ggiNEXT" && !is.null(O$ipt)) return(gg(iNEXT::ggiNEXT(O$ipt, type = 1, se = isTRUE(e$faixa)) + ggplot2::scale_colour_manual(values = rep(pal, 3)) +
      ggplot2::scale_fill_manual(values = rep(pal, 3)) + titulos_gg(e, chave, tt, "Número de indivíduos", "Riqueza de espécies") + tema_gg(e)))
    return(base(function() {
      vegan::rarecurve(mm, step = 1, col = rep(pal, length.out = nrow(mm)), lwd = 1.6 * lw, label = TRUE, xlab = rot(e, chave, "x", "Número de indivíduos"), ylab = rot(e, chave, "y", "Riqueza de espécies"))
      graphics::abline(v = min(rowSums(mm)), lty = 3, col = k$tinta2)
      titulo_base(e, chave, tt); nota_base(e, chave, "vegan::rarecurve; linha pontilhada = menor amostra")
    }))
  }
  if (chave == "indices_pontos") {
    ip <- G$indices$tabela; tt <- paste("Diversidade por ponto amostral:", gr)
    if (forma == "renyi") {
      mm <- O$m[rowSums(O$m) > 0, , drop = FALSE]
      rr <- vegan::renyi(mm, scales = c(0, .25, .5, 1, 2, 4, 8, Inf), hill = TRUE)
      d <- data.frame(ponto = rep(rownames(rr), ncol(rr)), escala = factor(rep(colnames(rr), each = nrow(rr)), levels = colnames(rr)), v = as.vector(as.matrix(rr)))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(escala, v, colour = ponto, group = ponto)) + ggplot2::geom_line(linewidth = lw) + ggplot2::geom_point(size = pt) +
        ggplot2::scale_colour_manual(values = cores_pontos(rownames(rr), pal)) + titulos_gg(e, chave, paste("Perfil de diversidade (números de Hill):", gr), "Ordem q (0 = riqueza, 1 = Shannon, 2 = Simpson)", "Espécies efetivas",
        "vegan::renyi(hill = TRUE). Curvas que não se cruzam permitem ordenar os pontos sem depender do índice.") + ggplot2::labs(colour = NULL) + tema_gg(e)))
    }
    if (forma == "base") return(base(function() {
      graphics::par(mfrow = c(1, 2), oma = c(0, 0, if (isTRUE(e$mostrar_titulo)) 1.5 else 0, 0))
      graphics::barplot(ip$S, names.arg = ip$ponto, col = pal[1], border = NA, ylab = "Riqueza", main = "Riqueza")
      graphics::barplot(ip$shannon, names.arg = ip$ponto, col = pal[2], border = NA, ylab = "Shannon (H')", main = "Shannon")
      if (isTRUE(e$mostrar_titulo)) graphics::mtext(rot(e, chave, "titulo", tt), outer = TRUE, font = 2, family = e$familia_titulo)
    }))
    lg <- rbind(data.frame(ponto = ip$ponto, medida = "Riqueza", v = ip$S), data.frame(ponto = ip$ponto, medida = "Shannon (H')", v = ip$shannon),
                data.frame(ponto = ip$ponto, medida = "Simpson (1 - D)", v = ip$simpson), data.frame(ponto = ip$ponto, medida = "Pielou (J')", v = ip$pielou))
    if (forma == "hill") {
      nh <- c("Riqueza", "Shannon", "Simpson")
      dh <- rbind(data.frame(ponto = ip$ponto, h = nh[1], v = ip$hill0), data.frame(ponto = ip$ponto, h = nh[2], v = ip$hill1), data.frame(ponto = ip$ponto, h = nh[3], v = ip$hill2))
      dh$h <- factor(dh$h, levels = nh); dh$ponto <- factor(dh$ponto, levels = ip$ponto)
      return(gg(ggplot2::ggplot(dh, ggplot2::aes(ponto, v, fill = h)) + ggplot2::geom_col(position = ggplot2::position_dodge(.8), width = .75, na.rm = TRUE) +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(abs(v - round(v)) < 1e-9, formatC(round(v), format = "d"), fmt1(v))), position = ggplot2::position_dodge(.8), vjust = -.4, family = e$familia, size = e$fonte / 3.4, colour = k$tinta, na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = pal[1:3], name = NULL) + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
        titulos_gg(e, chave, "Diversidade por ponto", NULL, "Número efetivo de espécies",
                   "Quanto mais perto as três colunas, mais equilibrada a comunidade; colunas de Shannon e Simpson baixas = poucas espécies dominam.") + tema_gg(e), w * 1.1, h))
    }
    if (forma == "pontos") return(gg(ggplot2::ggplot(lg, ggplot2::aes(ponto, v, colour = ponto)) + ggplot2::geom_segment(ggplot2::aes(xend = ponto, y = 0, yend = v), linewidth = lw, na.rm = TRUE) +
      ggplot2::geom_point(size = pt * 1.5, na.rm = TRUE) + ggplot2::facet_wrap(~medida, scales = "free_y") + ggplot2::scale_colour_manual(values = cores_pontos(ip$ponto, pal), guide = "none") +
      titulos_gg(e, chave, tt, NULL, NULL) + tema_gg(e), w * 8 / 7, h * 5.5 / 4.5))
    return(gg(ggplot2::ggplot(lg, ggplot2::aes(ponto, v, fill = ponto)) + ggplot2::geom_col(width = .66, show.legend = FALSE, na.rm = TRUE) + ggplot2::facet_wrap(~medida, scales = "free_y") +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(v >= 10 | v == round(v), formatC(v, format = "f", digits = 0), formatC(v, format = "f", digits = 2, decimal.mark = ","))), vjust = -.4, family = e$familia, size = e$fonte / 3.4, colour = k$tinta, na.rm = TRUE) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
      ggplot2::scale_fill_manual(values = cores_pontos(ip$ponto, pal)) + titulos_gg(e, chave, tt, NULL, NULL) + tema_gg(e), w * 8 / 7, h * 5.5 / 4.5))
  }
  if (chave == "similaridade") {
    if (is.null(G$similaridade)) return(invisible())
    S <- as.matrix(G$similaridade$jaccard); dimnames(S) <- list(G$similaridade$dendro_jaccard$rotulos, G$similaridade$dendro_jaccard$rotulos)
    tt <- paste("Similaridade de Jaccard entre os pontos:", gr)
    if (forma == "ladrilhos") {
      d <- data.frame(a = rep(rownames(S), ncol(S)), b = rep(colnames(S), each = nrow(S)), v = as.vector(S))
      return(gg(ggplot2::ggplot(d, ggplot2::aes(a, b, fill = v)) + ggplot2::geom_tile(colour = k$fundo, linewidth = 1) +
        ggplot2::geom_text(ggplot2::aes(label = formatC(v, format = "f", digits = 2, decimal.mark = ",")), family = e$familia, size = e$fonte / 3.2, colour = ifelse(d$v > .5, "white", k$tinta)) +
        ggplot2::scale_fill_gradient(low = "#eef2f8", high = pal[1], limits = c(0, 1), name = "Jaccard") + ggplot2::coord_equal() +
        titulos_gg(e, chave, tt, NULL, NULL) + tema_gg(e), h * 1.25, h))
    }
    h1 <- recriar_hclust(G$similaridade$dendro_jaccard)
    return(base(function() {
      stats::heatmap(S, Rowv = stats::as.dendrogram(h1), Colv = "Rowv", symm = TRUE, scale = "none", margins = c(4, 4),
                     col = grDevices::colorRampPalette(c("#f4f1e8", pal[1]))(40), main = if (isTRUE(e$mostrar_titulo)) rot(e, chave, "titulo", tt) else NULL)
    }))
  }
  if (chave %in% c("dendrograma_jaccard", "dendrograma_bray")) {
    if (is.null(G$similaridade)) return(invisible())
    d <- G$similaridade[[sub("dendrograma_", "dendro_", chave)]]; h1 <- recriar_hclust(d)
    nome <- if (chave == "dendrograma_bray") "Bray-Curtis" else "Jaccard"
    tt <- paste0("Dendrograma (", nome, ", UPGMA): ", gr); nota <- paste("Correlação cofenética =", formatC(d$cofenetica, format = "f", digits = 2, decimal.mark = ","))
    if (forma == "ggplot") {
      sg <- dendro_segmentos(d)
      return(gg(ggplot2::ggplot() + ggplot2::geom_segment(data = sg$seg, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), colour = pal[1], linewidth = lw) +
        ggplot2::geom_text(data = sg$folhas, ggplot2::aes(x = x, y = -0.03, label = rotulo), vjust = 1, family = e$familia, size = e$fonte / 3) +
        ggplot2::scale_x_continuous(NULL, breaks = NULL, expand = ggplot2::expansion(add = .6)) + ggplot2::scale_y_continuous(limits = c(-0.08, max(1, max(sg$seg$y)))) +
        titulos_gg(e, chave, tt, NULL, "Dissimilaridade", nota) + tema_gg(e)))
    }
    return(base(function() {
      dd <- stats::as.dendrogram(h1)
      if (forma == "horizontal") { graphics::par(mar = c(4.5, 2, if (isTRUE(e$mostrar_titulo)) 3.2 else 1, 4)); plot(dd, horiz = TRUE, xlab = rot(e, chave, "x", "Dissimilaridade"), edgePar = list(col = pal[1], lwd = 2 * lw)) }
      else if (forma == "triangulo") plot(dd, type = "triangle", ylab = rot(e, chave, "y", "Dissimilaridade"), edgePar = list(col = pal[1], lwd = 2 * lw))
      else { plot(h1, hang = -1, main = "", sub = "", xlab = "", ylab = rot(e, chave, "y", "Dissimilaridade"), lwd = 2 * lw, cex = 1.1)
             if (length(h1$labels) >= 4) stats::rect.hclust(h1, k = 2, border = pal[2]) }
      titulo_base(e, chave, tt); nota_base(e, chave, nota)
    }))
  }
  if (chave == "whittaker") {
    ra <- G$abundancia$tabela; tt <- paste("Curva de dominância (Whittaker):", gr)
    if (forma == "radfit") {
      v <- setNames(ra$n, ra$especie); rf <- vegan::radfit(v)
      return(base(function() {
        plot(rf, pch = 19, xlab = rot(e, chave, "x", "Posição da espécie"), ylab = rot(e, chave, "y", "Abundância (log)"))
        titulo_base(e, chave, tt); nota_base(e, chave, paste0("vegan::radfit; melhor modelo pelo AIC: ", names(which.min(sapply(rf$models, AIC)))))
      }))
    }
    if (forma == "barras") return(gg(ggplot2::ggplot(ra, ggplot2::aes(stats::reorder(especie, -n), n)) + ggplot2::geom_col(fill = pal[2], width = .7) +
      titulos_gg(e, chave, paste("Indivíduos por espécie:", gr), NULL, "Indivíduos") + tema_gg(e) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, face = "italic", family = e$familia_titulo)), w * 1.15, h * 1.2))
    return(gg(ggplot2::ggplot(ra, ggplot2::aes(posicao, relativa)) + ggplot2::geom_line(colour = pal[2], linewidth = lw) + ggplot2::geom_point(colour = pal[2], size = pt) +
      ggplot2::scale_y_log10(labels = function(x) paste0(format(100 * x, decimal.mark = ",", drop0trailing = TRUE), "%")) +
      titulos_gg(e, chave, tt, "Posição da espécie (mais para menos abundante)", "Abundância relativa (log)") + tema_gg(e)))
  }
  invisible()
}

dendro_segmentos <- function(d) {
  n <- length(d$rotulos); pos <- list(); seg <- list()
  for (i in seq_along(d$ordem)) pos[[paste0("f", d$ordem[i])]] <- c(i, 0)
  pega <- function(v) if (v < 0) pos[[paste0("f", -v)]] else pos[[paste0("c", v)]]
  mg <- matrix(unlist(d$merge), ncol = 2)
  for (kk in seq_len(nrow(mg))) {
    a <- pega(mg[kk, 1]); b <- pega(mg[kk, 2]); hh <- d$altura[kk]
    seg[[length(seg) + 1]] <- data.frame(x = c(a[1], b[1], a[1]), y = c(a[2], b[2], hh), xend = c(a[1], b[1], b[1]), yend = c(hh, hh, hh))
    pos[[paste0("c", kk)]] <- c((a[1] + b[1]) / 2, hh)
  }
  list(seg = do.call(rbind, seg), folhas = data.frame(x = seq_len(n), rotulo = unlist(d$rotulos)[unlist(d$ordem)]))
}

tag_grupo <- function(g) gsub("[^A-Za-z]", "", iconv(g, "UTF-8", "ASCII//TRANSLIT"))
# andamento: linha "[ 45%] <etapa>", legível no terminal; o servidor do app lê, tira do log e mostra a barra
progresso <- function(p, etapa) { cat(sprintf("[%3d%%] %s\n", as.integer(round(p)), etapa)); flush(stdout()) }

# uma cor por ponto de campo; com várias campanhas ("P1 C1", "P1 C2"), a mesma cor clareando nas campanhas mais antigas
cores_pontos <- function(pontos, pal) {
  campo <- sub(" C[0-9]+$", "", pontos); u <- unique(campo); base <- setNames(pal[(seq_along(u) - 1) %% length(pal) + 1], u)
  camp <- suppressWarnings(as.integer(sub(".* C", "", pontos))); nc <- max(c(1, camp), na.rm = TRUE)
  claro <- function(cor, f) grDevices::rgb(t(1 - (1 - grDevices::col2rgb(cor) / 255) * f))
  setNames(vapply(seq_along(pontos), function(i) if (is.na(camp[i])) base[[campo[i]]] else claro(base[[campo[i]]], .45 + .55 * camp[i] / nc), ""), pontos)
}

pode_figura <- function(G) !isTRUE(G$insuficiente) && G$riqueza >= 3 && G$n_pontos >= 3 && G$n_individuos >= 20

# versão do código das figuras = data (segundos) do arquivo .R mais novo da pasta R/ do programa
versao_codigo <- function() {
  d <- if (exists("aqui", envir = globalenv())) get("aqui", envir = globalenv()) else "R"
  fs <- list.files(d, "^(saida.*|fitofisionomias|campanhas|figuras|ajustes_fig)\\.R$", full.names = TRUE)   # só os arquivos que desenham figuras
  if (!length(fs)) return(0)
  as.numeric(max(file.mtime(fs), na.rm = TRUE))
}
# figuras finais do laudo: uma por chave, na forma escolhida
gerar_figuras <- function(R, OBJ, saida, e, so = NULL, faixa = c(60, 85)) {
  suppressPackageStartupMessages(library(ggplot2)); registrar_fontes(e)
  dir.create(file.path(saida, "figuras"), showWarnings = FALSE, recursive = TRUE)
  if (is.null(so)) unlink(list.files(file.path(saida, "figuras"), "\\.(png|jpg)$", full.names = TRUE))
  cat <- catalogo_todo(); figs <- list()
  gs <- names(R)[vapply(R, pode_figura, TRUE)]; tot <- length(gs) * length(cat); n <- 0
  for (g in names(R)) if (pode_figura(R[[g]])) {
    feitos <- character()
    for (ch in names(cat)) {
      n <- n + 1; if (is.null(so)) progresso(faixa[1] + diff(faixa) * (n - 1) / tot, sprintf("Figura %d de %d: %s (%s)", n, tot, cat[[ch]]$nome, g))
      if (!is.null(so) && ch != so) { if (file.exists(file.path(saida, "figuras", paste0(tag_grupo(g), "_", ch, ".png")))) feitos <- c(feitos, paste0(tag_grupo(g), "_", ch, ".png")); next }
      forma <- e$formas[[ch]] %||% cat[[ch]]$padrao
      arq <- file.path(saida, "figuras", paste0(tag_grupo(g), "_", ch, ".png"))
      ok <- tryCatch({ desenhar(ch, forma, R[[g]], OBJ[[g]], e, arq, e$largura, e$altura, e$dpi); file.exists(arq) },
                     error = function(x) { message("figura ", ch, " (", g, "): ", conditionMessage(x)); FALSE })
      if (isTRUE(ok)) feitos <- c(feitos, basename(arq))
    }
    figs[[g]] <- feitos
  }
  if (is.null(so)) jsonlite::write_json(list(codigo = versao_codigo(), feito_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                                        file.path(saida, "figuras", "_versao.json"), auto_unbox = TRUE)
  figs
}

# miniaturas de todas as formas, para escolher no painel
gerar_formas <- function(R, OBJ, saida, e, faixa = c(85, 95)) {
  suppressPackageStartupMessages(library(ggplot2)); registrar_fontes(e)
  dir.create(file.path(saida, "formas"), showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(file.path(saida, "formas"), glob2rx("*.png"), full.names = TRUE))
  cat <- catalogo_todo(); out <- list()
  gs <- names(R)[vapply(R, pode_figura, TRUE)]; tot <- length(gs) * sum(lengths(lapply(cat, `[[`, "formas"))); n <- 0
  for (g in names(R)) if (pode_figura(R[[g]])) {
    out[[g]] <- list()
    for (ch in names(cat)) for (f in names(cat[[ch]]$formas)) {
      n <- n + 1; if (n %% 4 == 1) progresso(faixa[1] + diff(faixa) * (n - 1) / tot, sprintf("Miniaturas das outras formas: %d de %d", n, tot))
      arq <- file.path(saida, "formas", paste0(tag_grupo(g), "_", ch, "__", f, ".png"))
      ok <- tryCatch({ desenhar(ch, f, R[[g]], OBJ[[g]], e, arq, 7, 4.5, 80); file.exists(arq) }, error = function(x) FALSE)
      if (isTRUE(ok)) out[[g]][[ch]] <- c(out[[g]][[ch]], f)
    }
  }
  out
}

# tabela de índices como a do PAST: uma linha por índice, uma coluna por ponto
indices_formato_past <- function(tab) {
  if (is.null(tab) || !nrow(tab)) return(NULL)
  f <- function(x, d) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, decimal.mark = ","))
  D <- if (!is.null(tab$dominancia_D)) tab$dominancia_D else 1 - tab$simpson
  out <- data.frame(`Índices de Diversidade` = c("Riqueza", "Abundância", "Dominance (D)", "Shannon (H)", "Equitability (J)"), check.names = FALSE, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(tab))) out[[tab$ponto[i]]] <- c(as.character(tab$S[i]), as.character(tab$N[i]), f(D[i], 4), f(tab$shannon[i], 3), f(if (tab$S[i] > 1) tab$pielou[i] else NA, 4))
  out
}
exportar_tabelas <- function(L, conf, R, saida) {
  pasta <- file.path(saida, "tabelas"); dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(pasta, glob2rx("*.xlsx"), full.names = TRUE))
  esc <- function(sh, arq) { sh <- sh[!vapply(sh, is.null, TRUE)]; names(sh) <- substr(names(sh), 1, 31); if (length(sh)) writexl::write_xlsx(sh, file.path(pasta, arq)) }
  esc(list("Conferência" = conf, "Completados pela consolidada" = if (!is.null(L$completados) && nrow(L$completados)) L$completados), "00_conferencia_da_planilha.xlsx")
  tudo <- list("Conferência" = conf)
  for (g in names(R)) {
    G <- R[[g]]; t <- tag_grupo(g); D <- G$descritiva
    ab <- G$matriz; mm <- as.data.frame(ab$valores); names(mm) <- ab$especies; mm <- cbind(Ponto = ab$pontos, mm)
    desc <- if (!is.null(D)) c(list("Lista de espécies" = D$especies, "Por família" = D$familias, "Por ponto" = D$pontos, "Forma de registro" = D$forma_registro),
                               if (length(D$categorias)) list("Atributos" = do.call(rbind, D$categorias))) else list()
    esc(c(desc, list("Matriz ponto x espécie" = mm)), sprintf("01_%s_descritiva.xlsx", t))
    if (isTRUE(G$insuficiente)) next
    esc(list("Estimadores (final)" = G$acumulacao$estimadores, "Estimadores por ponto" = G$acumulacao$poolaccum, "Curva de acumulação" = G$acumulacao$curva),
        sprintf("02_%s_riqueza_estimadores.xlsx", t))
    esc(list("iNEXT assintótico" = if (!is.null(G$inext$assintotico_abund)) rbind(cbind(base = "indivíduos", G$inext$assintotico_abund), if (!is.null(G$inext$assintotico_inc)) cbind(base = "pontos", G$inext$assintotico_inc)),
             "iNEXT indivíduos" = G$inext$abundancia, "iNEXT pontos" = G$inext$incidencia, "Rarefação por ponto" = G$rarefacao_pontos), sprintf("03_%s_rarefacao_inext.xlsx", t))
    esc(list("Índices (formato PAST)" = indices_formato_past(G$indices$tabela), "Índices por ponto" = G$indices$tabela, "Hutcheson pares" = G$hutcheson$pares, "Shannon com IC" = G$hutcheson$ic), sprintf("04_%s_diversidade_pontos.xlsx", t))
    if (!is.null(G$similaridade)) {
      mat <- function(M) cbind(Ponto = rownames(M), as.data.frame(M))
      esc(list("Pares de pontos" = G$similaridade$pares, "Jaccard" = mat(G$similaridade$jaccard), "Bray-Curtis" = mat(G$similaridade$bray),
               "Bray-Curtis raiz" = mat(G$similaridade$bray_raiz), "Complementaridade" = mat(G$similaridade$complementaridade), "Espécies em comum" = mat(G$similaridade$compartilhadas),
               "Beta resumo" = if (!is.null(G$beta)) data.frame(medida = c("Gama (riqueza total)", "Alfa médio", "Beta de Whittaker", "Sørensen (total)", "Substituição (Simpson)", "Aninhamento"),
                                                                 valor = c(G$beta$gama, G$beta$alfa_media, G$beta$whittaker, G$beta$sorensen, G$beta$turnover, G$beta$aninhamento)),
               "Beta substituição pares" = if (!is.null(G$beta)) mat(G$beta$par_turnover)), sprintf("05_%s_similaridade_beta.xlsx", t))
    }
    esc(list("Ranque de abundância" = G$abundancia$tabela), sprintf("06_%s_dominancia.xlsx", t))
    if (!is.null(G$fitofisionomias)) { F <- G$fitofisionomias
      esc(list("Por fitofisionomia" = F$classes$tabela, "Espécies por fitofisionomia" = F$classes$especies, "Pares de fitofisionomias" = F$classes$pares,
               "Famílias por fitofisionomia" = F$classes$familias, "Rarefação (iNEXT)" = F$classes$rarefacao,
               "Por sigla (componente)" = F$componentes$tabela, "Espécies por sigla" = F$componentes$especies, "Pares de siglas" = F$componentes$pares), sprintf("08_%s_fitofisionomias.xlsx", t)) }
    if (!is.null(G$campanhas)) esc(list("Resumo por campanha" = G$campanhas$resumo, "Espécies por campanha" = G$campanhas$especies,
      "Pares de campanhas" = G$campanhas$pares, "Cada ponto nas campanhas" = G$campanhas$por_ponto,
      "NMDS" = G$nmds$pontos, "PERMANOVA" = if (!is.null(G$permanova$R2)) as.data.frame(G$permanova[c("grupos", "R2", "F", "p", "p_dispersao")])), sprintf("07_%s_campanhas.xlsx", t))
    tudo[[paste(g, "- matriz")]] <- mm; tudo[[paste(g, "- índices")]] <- G$indices$tabela; tudo[[paste(g, "- estimadores")]] <- G$acumulacao$estimadores
  }
  invisible(list.files(pasta))
}

# distâncias entre pontos a partir de "22 K 668628.87 m E 8080967.81 m S" ou "-14.75° -47.95°"
distancias_pontos <- function(P) {
  if (is.null(P) || nrow(P) < 2 || is.null(P$coordenadas)) return(NULL)
  coords <- lapply(P$coordenadas, function(s) if (!is.na(s) && nzchar(s)) ler_coordenada(s))
  D <- matrix(NA_real_, nrow(P), nrow(P), dimnames = list(P$ponto, P$ponto))
  for (i in seq_along(coords)) {
    a <- coords[[i]]
    if (is.null(a) || !is.na(a$problema) || is.na(a$crs)) next
    D[i, i] <- 0
    for (j in seq_len(i - 1L)) {
      b <- coords[[j]]
      if (is.null(b) || !is.na(b$problema) || !identical(a$crs, b$crs)) next
      if (a$crs == "graus") {
        rad <- pi / 180
        h <- sin((a$y - b$y) * rad / 2)^2 + cos(a$y * rad) * cos(b$y * rad) * sin((a$x - b$x) * rad / 2)^2
        d <- 2 * 6371000 * asin(sqrt(min(1, max(0, h))))
      } else d <- sqrt((a$x - b$x)^2 + (a$y - b$y)^2)
      D[i,j] <- D[j,i] <- round(d)
    }
  }
  if (any(is.finite(D[upper.tri(D)]))) D else NULL
}
