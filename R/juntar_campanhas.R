# Quadrante: junta planilhas de campanhas diferentes numa planilha nova, com a coluna Campanha.
# Uso: Rscript juntar_campanhas.R <saida.xlsx> "<nome campanha 1>" <planilha1.xlsx> "<nome campanha 2>" <planilha2.xlsx> [...]
# - Planilha com aba de registros (brutos): os registros entram como estão.
# - Planilha só com a aba consolidada (espécie x P1..Pn): cada célula com número vira um registro (Total),
#   com família e grupo tirados das linhas de título da consolidada; VO e AV ficam vazios.
# - Pontos: a aba de pontos da ÚLTIMA planilha que tiver uma.
# - Consolidada nova: união das espécies, totais por ponto SOMADOS entre campanhas e atributos
#   (nome comum, End., Háb...) da campanha mais recente que os tiver. As originais não são alteradas.

# pacotes do R: pasta r-pacotes do programa (criada pelo instalar.cmd), ou HERP_RLIB, ou a biblioteca da máquina
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
suppressPackageStartupMessages({ library(readxl); library(writexl) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(aqui, "ler.R"), encoding = "UTF-8")
a <- commandArgs(TRUE)
if (length(a) < 5 || (length(a) - 1) %% 2) stop("Uso: juntar_campanhas.R saida.xlsx \"Campanha 1\" planilha1.xlsx \"Campanha 2\" planilha2.xlsx")
saida <- a[1]; camps <- a[seq(2, length(a), 2)]; arqs <- a[seq(3, length(a), 2)]

if (any(!nzchar(trimws(camps))) || anyDuplicated(camps)) stop("Informe um nome distinto para cada campanha.")
registros <- list(); pontos <- NULL; consols <- list(); notas <- character()
for (i in seq_along(arqs)) {
  tmp <- copiar_planilha(normalizePath(arqs[i], mustWork = TRUE)); abas <- excel_sheets(tmp)
  L <- ler_planilha(tmp)
  if (!is.null(L$pontos) && nrow(L$pontos)) {
    cadastro <- L$pontos
    cadastro$campanha <- camps[i]
    if (!is.null(pontos)) {
      for (nm in setdiff(names(pontos), names(cadastro))) cadastro[[nm]] <- NA_real_
      for (nm in setdiff(names(cadastro), names(pontos))) pontos[[nm]] <- NA_real_
    }
    pontos <- rbind(pontos, cadastro)
  }
  a_con <- L$aba_consolidada
  if (length(unique(L$registros$campanha)) > 1) stop("A planilha já contém mais de uma campanha: ", arqs[i])
  r <- L$registros
  if (!nrow(r)) stop("Campanha sem registros primários em ", arqs[i])
  if (any(!total_de_campo_valido(r$total)) || any(is.na(r$ponto) | !nzchar(r$ponto)) || any(is.na(r$especie) | !nzchar(r$especie)))
    stop("Corrija os pontos, espécies e contagens inválidos antes de juntar a campanha ", camps[i], ".")
  registros[[i]] <- data.frame(Campanha = camps[i], Ponto = r$ponto_original, `Espécie (nome válido)` = r$especie_completa,
    Família = r$familia, Grupo = r$grupo, VO = r$vo, AV = r$av, `Sem forma anotada` = r$sem_forma, Total = r$total,
    Observação = r$observacao, Método = r$metodo, Estação = r$estacao, Guilda = r$guilda, Cinegética = r$cinegetica,
    Xerimbabo = r$xerimbabo, `Interesse médico` = r$medico, Exótica = r$exotica, Bioindicadora = r$bioindicadora,
    Migratória = r$migratoria, check.names = FALSE, stringsAsFactors = FALSE)
  notas <- c(notas, sprintf("%s: %d registros primários de %s, aba %s.", camps[i], nrow(r), basename(arqs[i]), L$aba_registros))
  if (!is.na(a_con)) consols[[i]] <- ler_consolidada(tmp, a_con)
}
reg <- do.call(rbind, registros)
# grupo como texto padronizado
reg$Grupo <- grupo_padrao(reg$Grupo)

# consolidada unida (totais somados; atributos da campanha mais recente que tiver)
pts <- sort(unique(norm_ponto(reg$Ponto))); pts <- pts[order(as.integer(sub("\\D+", "", pts)))]
reg$esp <- binomio(reg$`Espécie (nome válido)`)
ordem <- reg[!duplicated(reg$esp), c("Grupo", "Família", "esp")]
ordem <- ordem[order(ordem$Grupo, ordem$Família, ordem$esp), ]
atr <- c("nome_comum", "endemismo", "habito", "dependencia", "sensibilidade", "forma_registro", "salve", "iucn")
linhas <- list(c("Ordem/ Família/ Espécie", "Nome comum", "Pontos amostrais", rep("", length(pts) - 1), "", "End.", "Háb.", "Dep.", "Sens.", "Forma de Registro", "Status", ""),
               c("", "", pts, "Total", rep("", 5), "Salve", "IUCN"))
gant <- fant <- ""
for (j in seq_len(nrow(ordem))) {
  o <- ordem[j, ]; if (!identical(o$Grupo, gant)) { linhas[[length(linhas) + 1]] <- c(o$Grupo, rep("", length(pts) + 10)); gant <- o$Grupo; fant <- "" }
  if (!identical(o$`Família`, fant)) { linhas[[length(linhas) + 1]] <- c(o$`Família`, rep("", length(pts) + 10)); fant <- o$`Família` }
  rr <- reg[reg$esp == o$esp, ]; v <- vapply(pts, function(p) sum(rr$Total[norm_ponto(rr$Ponto) == p]), 0)
  at <- rep(NA_character_, length(atr)); nome <- rr$`Espécie (nome válido)`[1]
  for (q in rev(consols)) if (!is.null(q)) { w <- which(q$especie == o$esp)[1]; if (!is.na(w)) { at <- ifelse(is.na(at), unlist(q[w, atr]), at); nome <- q$especie_completa[w] %||% nome } }
  linhas[[length(linhas) + 1]] <- c(nome, at[1] %||% "", ifelse(v > 0, as.character(v), "-"), as.character(sum(v)), ifelse(is.na(at[2:8]), "", at[2:8]))
}
tot <- vapply(pts, function(p) sum(reg$Total[norm_ponto(reg$Ponto) == p]), 0)
linhas[[length(linhas) + 1]] <- c("Total", "", as.character(tot), as.character(sum(tot)), rep("", 7))
larg <- max(lengths(linhas)); cons <- as.data.frame(do.call(rbind, lapply(linhas, function(x) c(x, rep("", larg - length(x))))), stringsAsFactors = FALSE)

reg$esp <- NULL
notas <- c(paste("Planilha montada pelo Quadrante (juntar_campanhas.R) em", format(Sys.time(), "%d/%m/%Y %H:%M")), notas,
           "A aba 'Dados consolidados' desta planilha SOMA as campanhas por ponto; para os números de cada campanha, ver a coluna Campanha em 'Dados brutos (campo)'.",
           "As planilhas originais não foram alteradas.")
# primeira linha vira cabeçalho (nomes vazios repetidos viram espaços, para o Excel aceitar)
cabeca <- function(d, h = 1) { n <- ifelse(is.na(unlist(d[h, ])), "", unlist(d[h, ])); v <- which(n == ""); n[v] <- strrep(" ", seq_along(v))
  d <- d[-(1:h), , drop = FALSE]; names(d) <- n; rownames(d) <- NULL; d }
abas_saida <- list(`Dados consolidados` = cabeca(cons), `Dados brutos (campo)` = reg, Origem = data.frame(Nota = notas))
if (!is.null(pontos) && nrow(pontos)) {
  cadastro_saida <- data.frame(Ponto = pontos$nome, Fitofisionomia = pontos$fitofisionomia,
    Campanha = pontos$campanha, Coordenadas = pontos$coordenadas, Método = pontos$metodo,
    `Grupo amostral` = pontos$grupo_amostral, `Esforço (h)` = pontos$esforco, check.names = FALSE)
  for (nm in catalogo_gradientes(pontos)$nome) cadastro_saida[[nm]] <- pontos[[paste0("amb.", nm)]]
  abas_saida <- c(list(`Pontos Amostrais` = cadastro_saida), abas_saida)
}
write_xlsx(abas_saida, saida)
cat(paste(notas, collapse = "\n"), "\n", nrow(reg), "registros gravados em", saida, "\n")
