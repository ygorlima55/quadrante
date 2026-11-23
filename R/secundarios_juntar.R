# Junta na planilha do levantamento o que o agente de dados secundários trouxe (CSVs com ; em UTF-8:
# obras.csv, especies.csv, nomes_campo.csv, busca.csv). Linhas com revisado_a_mao = sim nunca mudam.
# Uso: Rscript R/secundarios_juntar.R <pasta do levantamento> <pasta com os CSVs>
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("ler.R", "conferir.R", "secundarios.R")) source(file.path(aqui, f), encoding = "UTF-8")
a <- commandArgs(TRUE)
if (length(a) < 2 || !dir.exists(a[1]) || !dir.exists(a[2])) stop("Uso: Rscript R/secundarios_juntar.R <pasta do levantamento> <pasta com os CSVs>")
arq <- file.path(a[1], "secundarios", "dados_secundarios.xlsx")
r <- juntar_secundarios(arq, a[2])
cat(sprintf("Planilha atualizada: %s\nObras novas: %d | obras atualizadas: %d | espécies novas: %d | espécies atualizadas: %d | linhas revisadas à mão preservadas: %d\n",
            arq, r$obras_novas, r$obras_atualizadas, r$especies_novas, r$especies_atualizadas, r$preservadas))
