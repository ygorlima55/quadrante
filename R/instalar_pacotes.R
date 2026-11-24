# Instala os pacotes do Quadrante na pasta r-pacotes do programa (versões travadas por data do CRAN).
# Uso: Rscript R/instalar_pacotes.R   (o instalar.cmd faz isso sozinho)
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
lib  <- if (nzchar(Sys.getenv("HERP_RLIB"))) Sys.getenv("HERP_RLIB") else file.path(dirname(aqui), "r-pacotes")
repo <- "https://packagemanager.posit.co/cran/2026-11-16"   # foto do CRAN de 16/11/2026: todos instalam as mesmas versões
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
pkgs <- c("vegan", "iNEXT", "ggplot2", "readxl", "writexl", "jsonlite", "base64enc", "glmmTMB", "cowplot")   # glmmTMB e cowplot: modo Seca e chuva (24/11/2026)  # ggpattern é opcional: só o tema "Síntese" usa as hachuras
falta <- pkgs[!vapply(pkgs, requireNamespace, TRUE, quietly = TRUE)]
if (length(falta)) {
  cat("[ 55%] Baixando e instalando:", paste(falta, collapse = ", "), "(e o que eles precisam). Leva alguns minutos.\n")
  install.packages(falta, lib = lib, repos = repo, type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"),
                   dependencies = c("Depends", "Imports", "LinkingTo"))
}
cat("[ 95%] Conferindo os pacotes\n")
ok <- vapply(pkgs, requireNamespace, TRUE, quietly = TRUE)
for (p in pkgs) cat(sprintf("  %-10s %s\n", p, if (ok[[p]]) as.character(packageVersion(p)) else "NÃO INSTALOU"))
if (!all(ok)) { cat("\nAlgum pacote não instalou. Confira a internet e rode o instalar.cmd de novo.\n"); quit(status = 1) }
ip <- installed.packages(lib.loc = lib)[, c("Package", "Version", "License"), drop = FALSE]
if (nrow(ip)) write.csv(ip, file.path(lib, "pacotes_instalados.csv"), row.names = FALSE)
cat("\nPacotes prontos.\n")
