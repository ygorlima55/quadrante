# Quadrante: ajustes de uma figura pedidos no painel (19/11/2026, pedido do usuário: "editar o que tem na legenda e
# remover qualquer elemento da figura"). Vale para as figuras feitas com ggplot2 (a maioria); as do R base
# (heatmap, plot(hclust), rarecurve, radfit) não aceitam estes ajustes e o painel avisa.
# a = e$ajustes[[chave]] = list(
#   ocultar = c("legenda", "titulo", "titulo_x", "titulo_y", "nomes_x", "nomes_y", "rotulos", "pontos", "linhas_ref", "barras_erro", "faixa", "titulo_legenda"),
#   remover = c("item da legenda", ...),          # some da legenda E do desenho (as linhas com esse valor saem dos dados da figura)
#   nomes   = list("texto antigo" = "texto novo"), # troca o texto do item na legenda
#   titulo_legenda = "texto")

GEOMS_OCULTAVEIS <- list(rotulos = c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel"), pontos = "GeomPoint",
  linhas_ref = c("GeomHline", "GeomVline", "GeomAbline"), barras_erro = c("GeomErrorbar", "GeomErrorbarh", "GeomLinerange", "GeomPointrange", "GeomCrossbar"),
  faixa = c("GeomRibbon", "GeomArea"))

ajustar_fig <- function(p, a) {
  if (is.null(a) || !length(a) || !inherits(p, "ggplot")) return(p)
  oc <- unlist(a$ocultar); rem <- unlist(a$remover); nomes <- unlist(a$nomes)
  nomes <- nomes[nzchar(names(nomes) %||% "") & nzchar(nomes)]
  # 1. camadas inteiras (rótulos de valor, pontos, linhas de referência, barras de erro, faixas)
  tira <- unlist(GEOMS_OCULTAVEIS[intersect(oc, names(GEOMS_OCULTAVEIS))])
  if (length(tira)) {
    ly <- Filter(function(l) !any(class(l$geom) %in% tira), p$layers)
    if (length(ly)) p$layers <- ly   # nunca deixar a figura sem nenhuma camada
  }
  # 2. itens removidos: saem dos dados da figura e de cada camada
  if (length(rem)) {
    filtra <- function(d) {
      if (!is.data.frame(d) || !nrow(d)) return(d)
      fora <- rep(FALSE, nrow(d))
      for (cl in names(d)) if (is.character(d[[cl]]) || is.factor(d[[cl]])) fora <- fora | (as.character(d[[cl]]) %in% rem)
      d <- d[!fora, , drop = FALSE]
      for (cl in names(d)) if (is.factor(d[[cl]])) d[[cl]] <- droplevels(d[[cl]])
      d
    }
    p$data <- filtra(p$data)
    for (i in seq_along(p$layers)) if (is.data.frame(p$layers[[i]]$data)) p$layers[[i]]$data <- filtra(p$layers[[i]]$data)
  }
  # 3. textos dos itens da legenda: troca nos rótulos das escalas de cor, preenchimento, forma e tipo de linha
  if (length(nomes)) {
    troca <- function(x) { x <- as.character(x); i <- x %in% names(nomes); x[i] <- nomes[x[i]]; x }
    achou <- FALSE
    for (sc in p$scales$scales) if (any(sc$aesthetics %in% c("fill", "colour", "shape", "linetype")) && inherits(sc, "ScaleDiscrete")) {
      lb <- sc$labels
      sc$labels <- if (is.function(lb)) function(b) troca(lb(b)) else if (is.character(lb) && length(lb)) troca(lb) else function(b) troca(b)
      achou <- TRUE
    }
    if (!achou) {   # sem escala discreta declarada: troca nos dados (o texto muda também no eixo, se for o mesmo)
      muda <- function(d) { if (!is.data.frame(d)) return(d)
        for (cl in names(d)) if (is.factor(d[[cl]])) levels(d[[cl]]) <- troca(levels(d[[cl]])) else if (is.character(d[[cl]])) d[[cl]] <- troca(d[[cl]])
        d }
      p$data <- muda(p$data); for (i in seq_along(p$layers)) if (is.data.frame(p$layers[[i]]$data)) p$layers[[i]]$data <- muda(p$layers[[i]]$data)
    }
    # 21/11/2026: o mesmo nome também nos eixos com categorias (ex.: barras por família sem legenda)
    if (achou) {
      decl <- character()
      for (sc in p$scales$scales) if (any(sc$aesthetics %in% c("x", "y")) && inherits(sc, "ScaleDiscrete")) {
        lb <- sc$labels
        sc$labels <- if (is.function(lb)) function(b) troca(lb(b)) else if (is.character(lb) && length(lb)) troca(lb) else function(b) troca(b)
        decl <- c(decl, intersect(sc$aesthetics, c("x", "y")))
      }
      b <- tryCatch(ggplot2::ggplot_build(p), error = function(x) NULL)
      if (!is.null(b)) {
        if (!"x" %in% decl && isTRUE(b$layout$panel_scales_x[[1]]$is_discrete())) p <- p + ggplot2::scale_x_discrete(labels = function(v) troca(v))
        if (!"y" %in% decl && isTRUE(b$layout$panel_scales_y[[1]]$is_discrete())) p <- p + ggplot2::scale_y_discrete(labels = function(v) troca(v))
      }
    }
  }
  # 4. partes do desenho
  th <- list()
  if ("legenda" %in% oc) th$legend.position <- "none"
  if ("titulo_legenda" %in% oc) th$legend.title <- ggplot2::element_blank()
  if ("titulo_x" %in% oc) th$axis.title.x <- ggplot2::element_blank()
  if ("titulo_y" %in% oc) { th$axis.title.y <- ggplot2::element_blank(); th$axis.title.y.right <- ggplot2::element_blank() }
  if ("nomes_x" %in% oc) { th$axis.text.x <- ggplot2::element_blank(); th$axis.ticks.x <- ggplot2::element_blank() }
  if ("nomes_y" %in% oc) { th$axis.text.y <- ggplot2::element_blank(); th$axis.ticks.y <- ggplot2::element_blank(); th$axis.text.y.right <- ggplot2::element_blank() }
  if (length(th)) p <- p + do.call(ggplot2::theme, th)
  if ("titulo" %in% oc) p <- p + ggplot2::labs(title = NULL, subtitle = NULL)
  if (nzchar(a$titulo_legenda %||% "")) p <- p + ggplot2::labs(fill = a$titulo_legenda, colour = a$titulo_legenda, shape = a$titulo_legenda, linetype = a$titulo_legenda)
  p
}
