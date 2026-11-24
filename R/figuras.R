# Quadrante: refaz as figuras do laudo (e as miniaturas) com o estilo.json da pasta, sem recalcular as análises.
# Uso: Rscript figuras.R "<pasta do levantamento>" [chave]   (com chave: só aquela figura, sem miniaturas)
# pacotes do R: pasta r-pacotes do programa (criada pelo instalar.cmd), ou HERP_RLIB, ou a biblioteca da máquina
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
suppressPackageStartupMessages({ library(ggplot2); library(jsonlite); library(vegan); library(iNEXT) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && !is.list(a) && is.na(a))) b else a
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
source(file.path(aqui, "desenho.R"), encoding = "UTF-8"); source(file.path(aqui, "saida.R"), encoding = "UTF-8"); source(file.path(aqui, "ajustes_fig.R"), encoding = "UTF-8"); source(file.path(aqui, "saida_extra.R"), encoding = "UTF-8"); source(file.path(aqui, "campanhas.R"), encoding = "UTF-8"); source(file.path(aqui, "estacoes.R"), encoding = "UTF-8"); source(file.path(aqui, "saida_novas.R"), encoding = "UTF-8"); source(file.path(aqui, "fitofisionomias.R"), encoding = "UTF-8")
a <- commandArgs(TRUE); saida <- a[1]; so <- if (length(a) >= 2 && nzchar(a[2])) a[2] else NULL
x <- readRDS(file.path(saida, "resultados.rds"))
e <- ler_estilo(file.path(saida, "estilo.json"))
cat(format(Sys.time(), "%H:%M:%S"), if (is.null(so)) "Refazendo as figuras do laudo..." else paste("Refazendo a figura:", so), "\n")
f <- suppressMessages(gerar_figuras(x$R, x$objs, saida, e, so, faixa = if (is.null(so)) c(0, 70) else c(0, 100)))
if (is.null(so)) { cat(format(Sys.time(), "%H:%M:%S"), "Refazendo as miniaturas das outras formas...\n"); invisible(suppressMessages(gerar_formas(x$R, x$objs, saida, e, faixa = c(70, 100)))) }
cat(format(Sys.time(), "%H:%M:%S"), "Pronto.\n")
