# Gera exemplo/planilha_exemplo_seca_chuva_ficticia.xlsx: o formato que o Quadrante entende para o modo Seca e chuva.
# Parte da planilha de exemplo (fictícia) e inventa uma segunda campanha, a seca. NADA AQUI É DADO REAL.
# Uso: Rscript exemplo/gerar_exemplo_seca_chuva.R
local({
  aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
  if (nzchar(Sys.getenv("HERP_RLIB"))) .libPaths(c(Sys.getenv("HERP_RLIB"), .libPaths()))
  .libPaths(c(file.path(dirname(aqui), "r-pacotes"), "C:/DadosBio/envs/R-herpetodados", .libPaths()))
  set.seed(20261124)
  ori <- file.path(aqui, "planilha_exemplo_ficticia.xlsx")
  chuva <- as.data.frame(readxl::read_excel(ori, "Dados brutos (exemplo)"))
  pts   <- as.data.frame(readxl::read_excel(ori, "Pontos Amostrais"))
  chuva <- chuva[chuva$Grupo %in% c("Anuros", "Répteis"), ]   # só herpetofauna: o total sai como "Herpetofauna"
  chuva$Campanha <- "Chuva"

  # seca inventada: menos anuros vocalizando, alguns somem, répteis aparecem mais
  seca <- chuva
  anuro <- seca$Grupo == "Anuros"
  seca$VO[anuro] <- pmax(0, round(seca$VO[anuro] * runif(sum(anuro), .1, .6)))
  seca$AV <- pmax(0, round(seca$AV * runif(nrow(seca), .6, 1.6)))
  seca$Total <- seca$VO + seca$AV
  seca <- seca[seca$Total > 0 & !(seca$Ponto == "Ponto 06"), ]        # Ponto 06 foi visitado na seca, mas sem nenhum registro
  extra <- data.frame(Campanha = "Seca", Ponto = c("Ponto 04", "Ponto 05", "Ponto 05", "Ponto 02"),
    `Espécie` = c("Tropidurus oreadicus Rodrigues, 1987", "Ameiva ameiva (Linnaeus, 1758)", "Rhinella diptycha (Cope, 1862)", "Scinax fuscovarius (A. Lutz, 1925)"),
    `Família` = c("Tropiduridae", "Teiidae", "Bufonidae", "Hylidae"), Grupo = c("Répteis", "Répteis", "Anuros", "Anuros"),
    VO = c(0, 0, 0, 2), AV = c(3, 1, 2, 1), check.names = FALSE)
  extra$Total <- extra$VO + extra$AV
  seca$Campanha <- "Seca"
  dados <- rbind(chuva, seca[names(chuva)], extra[names(chuva)])

  # aba de pontos: uma linha por ponto (fitofisionomia, coordenadas, esforço, método) e, na coluna Campanha,
  # uma linha a mais para cada visita que não teve registro (aqui: Ponto 06 na seca)
  pts$Campanha <- NA_character_
  vis <- pts[pts$`Sítios amostrais` == "Ponto 06", ]; vis$Campanha <- "Seca"
  pts <- rbind(pts, vis)

  leia <- data.frame(`EXEMPLO FICTÍCIO: SECA E CHUVA` = c(
    "Nada aqui é dado real: os nomes das espécies são reais, mas pontos, contagens e a campanha seca são inventados.",
    "1. Aba 'Dados brutos': um registro por linha. A coluna Campanha diz de qual campanha é o registro: Chuva ou Seca.",
    "   O programa reconhece a estação pelo nome: chuva, chuvosa, úmida, cheia = chuva; seca, estiagem = seca. A chuva sempre vem primeiro.",
    "   Também vale 'Campanha 1 - chuvosa' e 'Campanha 2 - seca', ou uma coluna Estação (ou Sazonalidade) com Chuva/Seca.",
    "2. O ponto tem o MESMO nome nas duas campanhas (Ponto 01 na chuva e na seca): é assim que a comparação pareada por ponto funciona.",
    "3. Grupo: Anuros ou Répteis. A Herpetofauna é a soma de todos; Anuros e Répteis saem separados.",
    "4. Aba 'Pontos Amostrais': uma linha por ponto com fitofisionomia, coordenadas, esforço e método.",
    "   Ponto visitado numa campanha SEM nenhum registro: repita a linha do ponto e escreva a campanha na coluna Campanha (aqui, Ponto 06 na Seca).",
    "   Sem isso o programa acha que o ponto não foi visitado naquela campanha."), check.names = FALSE)
  saida <- file.path(aqui, "planilha_exemplo_seca_chuva_ficticia.xlsx")
  writexl::write_xlsx(list(`Leia-me` = leia, `Dados brutos (exemplo)` = dados, `Pontos Amostrais` = pts), saida)
  cat("Gravado:", saida, "\n"); print(table(dados$Campanha, dados$Grupo)); print(aggregate(Total ~ Campanha, dados, sum))
})
