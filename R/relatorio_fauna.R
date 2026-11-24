# Quadrante: análises complementares de comunidades de fauna.
#   1. estimadores por abundância (Chao 1 e ACE) com a porcentagem da riqueza já amostrada;
#   2. riqueza, abundância e espécies exclusivas por MÉTODO de registro;
#   3. taxas por ESFORÇO (indivíduos e espécies por hora), quando a aba de pontos traz "Esforço (h)";
#   4. quadro de ESPÉCIES DE INTERESSE (ameaçadas, quase ameaçadas, endêmicas, cinegéticas, xerimbabo, interesse médico,
#      exóticas, bioindicadoras, migratórias, raras na amostra);
#   5. SAZONALIDADE / campanhas: riqueza e abundância por campanha ou estação, com teste pareado por ponto;
#   6. IPA (índice pontual de abundância) para aves;
#   7. aviso de dados insuficientes e ficha de MÉTODOS (programa, versões, aleatorizações, unidade amostral).
# Só calcula e escreve tabelas; não desenha nada (o visual é de outra sessão).

sim_nao <- function(x) !is.na(x) & nzchar(x) & !grepl("^(n|nao|não|no|0|-|false)$", sem_acento(trimws(x)))
primeiro <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (length(v)) v[1] else NA_character_ }

# exóticas e de interesse médico reconhecidas sem depender da planilha (lista curta e conservadora; o resto vem das colunas)
EXOTICAS_CONHECIDAS <- c("Lithobates catesbeianus", "Hemidactylus mabouia", "Passer domesticus", "Columba livia", "Sturnus vulgaris",
                         "Rattus rattus", "Rattus norvegicus", "Mus musculus", "Sus scrofa", "Apis mellifera")
FAMILIAS_PECONHENTAS <- c("Viperidae", "Elapidae")

analisar_relatorio <- function(reg, m, nome, pontos_info = NULL, consol = NULL, campanhas = NULL) {
  out <- list()
  v <- colSums(m); v <- v[v > 0]; S <- length(v); N <- sum(v)

  # 1. Chao 1 e ACE (abundância)
  er <- tryCatch(vegan::estimateR(v), error = function(e) NULL)
  if (!is.null(er)) {
    out$estimadores_abundancia <- data.frame(
      estimador = c("Riqueza observada", "Chao 1", "ACE"),
      valor = round(c(er["S.obs"], er["S.chao1"], er["S.ACE"]), 2),
      erro_padrao = round(c(NA, er["se.chao1"], er["se.ACE"]), 2),
      completude = round(c(1, er["S.obs"] / er["S.chao1"], er["S.obs"] / er["S.ACE"]), 3), row.names = NULL)
    out$estimadores_abundancia$texto <- NULL
    out$texto_estimadores <- txt(
      "Chao 1 e ACE estimam a riqueza a partir das ABUNDÂNCIAS: usam as espécies com 1 e 2 indivíduos (Chao 1) ou com até 10 (ACE). Completam os estimadores por incidência (Chao 2, Jackknife, Bootstrap) que o programa já calcula por ponto.",
      sprintf("Observadas %d espécies; Chao 1 = %s (%s já amostradas) e ACE = %s (%s).", S, fmt(er["S.chao1"], 1), pct(er["S.obs"] / er["S.chao1"]),
              fmt(er["S.ACE"], 1), pct(er["S.obs"] / er["S.ACE"])),
      "Acima de 90% de completude, o inventário captou quase todas as espécies detectáveis com o método e a época usados.",
      "Contagens estimadas (coros, bandos) distorcem os estimadores por abundância; nesses casos, prefira Chao 2 e Jackknife, que usam só presença por ponto.")
  }

  # 2. por método de registro
  r <- reg[!is.na(reg$total) & reg$total > 0 & reg$especie %in% colnames(m), , drop = FALSE]
  if (any(!is.na(r$metodo) & nzchar(r$metodo))) {
    r$met <- ifelse(is.na(r$metodo) | !nzchar(r$metodo), "Não informado", r$metodo)
    mm <- tapply(r$total, list(r$met, r$especie), sum); mm[is.na(mm)] <- 0
    pres <- mm > 0
    tab <- data.frame(metodo = rownames(mm), riqueza = rowSums(pres), abundancia = rowSums(mm),
      pct_especies = round(rowSums(pres) / ncol(mm), 3),
      exclusivas = vapply(seq_len(nrow(mm)), function(i) sum(pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0), 1L),
      especies_exclusivas = vapply(seq_len(nrow(mm)), function(i) paste(colnames(mm)[pres[i, ] & colSums(pres[-i, , drop = FALSE]) == 0], collapse = "; "), ""),
      stringsAsFactors = FALSE, row.names = NULL)
    tab <- tab[order(-tab$riqueza), ]
    out$por_metodo <- tab
    out$metodo_especies <- cbind(especie = colnames(mm), as.data.frame(t(mm)), row.names = NULL)
    out$texto_metodo <- txt(
      "Resume quantas espécies e indivíduos cada método registrou e quais espécies apareceram exclusivamente em cada método.",
      sprintf("%s.", paste(sprintf("%s: %d espécies (%s), %d indivíduos, %d exclusivas", tab$metodo, tab$riqueza, pct(tab$pct_especies), tab$abundancia, tab$exclusivas), collapse = "; ")),
      "Métodos com muitas espécies exclusivas são complementares e não devem ser retirados do monitoramento.",
      "Métodos com esforços diferentes não se comparam pela riqueza bruta; informe o esforço de cada um (horas, baldes-noite, pontos de escuta).")
  }

  # 3. esforço (horas por ponto, da aba de pontos)
  if (!is.null(pontos_info) && any(!is.na(pontos_info$esforco))) {
    ef <- pontos_info$esforco[match(rownames(m), pontos_info$ponto)]
    valido <- is.finite(ef) & ef > 0
    ef[!valido] <- NA_real_
    tab <- data.frame(ponto = rownames(m), horas = ef, riqueza = rowSums(m > 0), abundancia = rowSums(m),
      individuos_por_hora = round(rowSums(m) / ef, 2), especies_por_hora = round(rowSums(m > 0) / ef, 2), stringsAsFactors = FALSE, row.names = NULL)
    tot <- if (all(valido)) sum(ef) else NA_real_
    out$esforco <- rbind(tab, data.frame(ponto = "Total", horas = tot, riqueza = S, abundancia = N,
      individuos_por_hora = round(N / tot, 2), especies_por_hora = round(S / tot, 2)))
    out$texto_esforco <- txt("Taxa de encontro por hora de amostragem, com as horas informadas para cada unidade.",
      if (all(valido)) sprintf("Esforço total de %s h. No conjunto, %s indivíduos por hora e %s espécies por hora.", fmt(tot, 1), fmt(N / tot), fmt(S / tot))
      else sprintf("Há esforço positivo informado para %d de %d unidades. As taxas aparecem para as unidades com esforço conhecido.", sum(valido), length(ef)),
      "A taxa depende do método, da atividade da fauna e da detectabilidade.",
      "A razão entre riqueza observada e horas descreve este levantamento. A riqueza não cresce linearmente com o esforço.")
  }

  # 4. espécies de interesse
  esp <- colnames(m); at <- function(col) vapply(esp, function(e) primeiro(reg[[col]][reg$especie == e]), "")
  k <- consol
  cat_ <- function(a) if (!is.null(k) && a %in% names(k)) toupper(trimws(as.character(k[[a]][match(esp, k$especie)]))) else rep(NA_character_, length(esp))
  salve <- cat_("salve"); iucn <- cat_("iucn"); endm <- cat_("endemismo")
  fam <- vapply(esp, function(e) primeiro(reg$familia[reg$especie == e]), "")
  if (!is.null(k) && "familia" %in% names(k)) { fk <- as.character(k$familia[match(esp, k$especie)]); fam <- ifelse(is.na(fam) | !nzchar(fam), fk, fam) }
  if (!is.null(k) && "familia_cons" %in% names(k)) { fk <- as.character(k$familia_cons[match(esp, k$especie)]); fam <- ifelse(is.na(fam) | !nzchar(fam), fk, fam) }
  ame <- function(x) !is.na(x) & grepl("^(VU|EN|CR)", x)
  qam <- function(x) !is.na(x) & grepl("^NT", x)
  ti <- data.frame(familia = fam, especie = esp, individuos = as.numeric(colSums(m)[esp]), stringsAsFactors = FALSE, row.names = NULL)
  ti$ameacada <- ifelse(ame(salve) | ame(iucn), paste0(ifelse(ame(salve), paste0("SALVE ", salve), ""), ifelse(ame(salve) & ame(iucn), "; ", ""), ifelse(ame(iucn), paste0("IUCN ", iucn), "")), "")
  ti$quase_ameacada <- ifelse(qam(salve) | qam(iucn), "sim", "")
  ti$endemica <- ifelse(!is.na(endm) & grepl("^E", endm), "sim", "")
  ti$cinegetica <- ifelse(sim_nao(at("cinegetica")), "sim", "")
  ti$xerimbabo <- ifelse(sim_nao(at("xerimbabo")), "sim", "")
  ti$interesse_medico <- ifelse(sim_nao(at("medico")) | fam %in% FAMILIAS_PECONHENTAS, "sim", "")
  ti$exotica <- ifelse(sim_nao(at("exotica")) | esp %in% EXOTICAS_CONHECIDAS, "sim", "")
  ti$bioindicadora <- ifelse(sim_nao(at("bioindicadora")), "sim", "")
  ti$migratoria <- ifelse(sim_nao(at("migratoria")), "sim", "")
  ti$rara_na_amostra <- ifelse(ti$individuos == 1, "1 indivíduo", "")
  flags <- c("ameacada", "quase_ameacada", "endemica", "cinegetica", "xerimbabo", "interesse_medico", "exotica", "bioindicadora", "migratoria", "rara_na_amostra")
  ti <- ti[apply(ti[flags], 1, function(x) any(nzchar(x))), , drop = FALSE]
  out$interesse <- ti[order(ti$familia, ti$especie), ]
  conta <- vapply(flags, function(f) sum(nzchar(out$interesse[[f]])), 1L)
  nomes_flag <- c(ameacada = "ameaçadas", quase_ameacada = "quase ameaçadas", endemica = "endêmicas", cinegetica = "cinegéticas", xerimbabo = "xerimbabo",
                  interesse_medico = "de interesse médico", exotica = "exóticas", bioindicadora = "bioindicadoras", migratoria = "migratórias", rara_na_amostra = "com 1 indivíduo só")
  singular_flag <- c(ameacada = "ameaçada", quase_ameacada = "quase ameaçada", endemica = "endêmica", cinegetica = "cinegética", xerimbabo = "xerimbabo",
                     interesse_medico = "de interesse médico", exotica = "exótica", bioindicadora = "bioindicadora", migratoria = "migratória", rara_na_amostra = "com 1 indivíduo só")
  out$texto_interesse <- txt(
    "Reúne num quadro as espécies que os termos de referência pedem para destacar: ameaçadas (listas nacional SALVE/MMA e IUCN), quase ameaçadas, endêmicas, cinegéticas, xerimbabo, de interesse médico, exóticas, bioindicadoras, migratórias e raras na amostra.",
    paste0(paste(sprintf("%d %s", conta[conta > 0], ifelse(conta[conta > 0] == 1, singular_flag[names(conta)[conta > 0]], nomes_flag[names(conta)[conta > 0]])), collapse = "; "), if (!any(conta > 0)) "Nenhuma espécie marcada." else "."),
    "Ameaçadas, endêmicas e exóticas costumam virar programas próprios no PBA; peçonhentas pedem orientação de segurança às equipes e aos trabalhadores.",
    "Interesse médico é marcado automaticamente para Viperidae e Elapidae; exóticas, por uma lista curta de espécies conhecidas. Cinegética, xerimbabo, bioindicadora e migratória só aparecem quando a planilha traz essas colunas. Lista estadual de ameaçadas: acrescente uma coluna própria na consolidada.")

  # 5. sazonalidade / campanhas: riqueza e abundância por ponto em cada campanha (ou estação), teste pareado
  comp_tempo <- function(rotulo, chave) {
    rr <- r[!is.na(r[[chave]]) & nzchar(r[[chave]]), , drop = FALSE]
    pc <- if (!is.null(rr$ponto_campo)) rr$ponto_campo else rr$ponto
    campo <- if (!is.null(reg$ponto_campo)) reg$ponto_campo else reg$ponto
    visitas <- unique(data.frame(ponto = campo, nivel = reg[[chave]]))
    if (chave == "campanha" && !is.null(pontos_info$campanha)) {
      visitas <- unique(rbind(visitas, data.frame(ponto = ponto_fisico(pontos_info$ponto), nivel = pontos_info$campanha)))
    }
    visitas <- visitas[!is.na(visitas$nivel) & nzchar(visitas$nivel), , drop = FALSE]
    niv <- sort(unique(visitas$nivel))
    if (length(niv) < 2) return(NULL)
    tab <- do.call(rbind, lapply(niv, function(x) { s <- rr[rr[[chave]] == x, ]
      data.frame(nivel = x, riqueza = length(unique(s$especie)), abundancia = sum(s$total), pontos = sum(visitas$nivel == x), stringsAsFactors = FALSE) }))
    teste <- NULL
    if (length(niv) == 2) {
      pts <- intersect(visitas$ponto[visitas$nivel == niv[1]], visitas$ponto[visitas$nivel == niv[2]])
      if (length(pts) >= 5) {
        sp <- function(x, p) length(unique(rr$especie[rr[[chave]] == x & pc == p]))
        a <- vapply(pts, function(p) sp(niv[1], p), 1); b <- vapply(pts, function(p) sp(niv[2], p), 1)
        if (any(a != b)) {
          w <- tryCatch(suppressWarnings(stats::wilcox.test(a, b, paired = TRUE, exact = TRUE)), error = function(e) NULL)
          if (!is.null(w) && is.finite(w$p.value)) teste <- data.frame(comparacao = paste(niv, collapse = " x "), pontos_pareados = length(pts), media_riqueza_1 = round(mean(a), 2),
            media_riqueza_2 = round(mean(b), 2), V = unname(w$statistic), p = w$p.value)
        }
      }
    }
    list(tipo = rotulo, tabela = tab, teste = teste)
  }
  st <- list()
  if (!is.null(r$camp_cod) || length(unique(r$campanha)) > 1) st$campanha <- comp_tempo("Campanha", "campanha")
  if (any(!is.na(r$estacao) & nzchar(r$estacao))) st$estacao <- comp_tempo("Estação (sazonalidade)", "estacao")
  st <- Filter(Negate(is.null), st)
  if (length(st)) {
    out$sazonal <- st
    tt <- unlist(lapply(st, function(s) c(sprintf("%s: %s", s$tipo, paste(sprintf("%s %d espécies e %d indivíduos", s$tabela$nivel, s$tabela$riqueza, s$tabela$abundancia), collapse = "; ")),
      if (!is.null(s$teste)) sprintf("teste de Wilcoxon pareado por ponto (%d pontos): p = %s", s$teste$pontos_pareados, fmt(s$teste$p, 3)))))
    out$texto_sazonal <- txt("Compara campanhas (ou estações chuvosa e seca) pela riqueza e abundância; com dois períodos e pelo menos 5 pontos amostrados nos dois, testa se a riqueza por ponto mudou (Wilcoxon pareado).",
      paste0(paste(tt, collapse = ". "), "."),
      "As diferenças descrevem os períodos amostrados. Considere a repetição de pontos, as datas e o esforço ao interpretar a variação temporal.",
      "Com poucos pontos o teste tem pouco poder; p alto não prova que as campanhas são iguais. Clima da noite e esforço também mudam entre campanhas.")
  }

  # 6. IPA para aves (contatos por unidade amostral)
  if (identical(nome, "Aves")) {
    ua <- nrow(m)
    out$ipa <- data.frame(especie = names(sort(v, decreasing = TRUE)), contatos = as.numeric(sort(v, decreasing = TRUE)),
                          IPA = round(as.numeric(sort(v, decreasing = TRUE)) / ua, 3), row.names = NULL)
    out$texto_ipa <- txt("Índice Pontual de Abundância (IPA): número de contatos da espécie dividido pelo número de pontos (unidades amostrais). É o índice de abundância mais usado para aves em relatórios.",
      sprintf("%d unidades amostrais. Maior IPA: %s (%s).", ua, out$ipa$especie[1], fmt(out$ipa$IPA[1], 2)),
      "IPA alto indica espécie comum ou muito detectável pelo canto.", "Supõe esforço igual em cada ponto; com pontos de escuta de tempos diferentes, o IPA fica enviesado.")
  }

  # 7. aviso de dados insuficientes
  if (N < 30 || S < 5) out$aviso <- sprintf("Poucos dados para %s (%d espécies, %d indivíduos): índices de diversidade, equitabilidade e estimadores ficam instáveis. Apresente a lista e a abundância e use os índices só como descrição.", nome, S, N)
  out
}

# ficha de métodos (rastreabilidade pedida pelos analistas dos órgãos)
ficha_metodos <- function(L, R) {
  pk <- c("vegan", "iNEXT", "ggplot2", "readxl", "ggpattern")
  v <- vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "não instalado"), "")
  data.frame(item = c("Programa", paste("Pacote", pk), "Unidade amostral", "Curva de acumulação", "Estimadores por incidência", "Estimadores por abundância",
                      "Rarefação e extrapolação", "Diversidade", "Equitabilidade", "Similaridade e agrupamento", "Dados secundários", "Planilha de origem"),
             valor = c(R.version.string, v,
                       if (!is.null(L$campanhas)) "ponto amostral, com as campanhas somadas (a comparação entre campanhas fica na seção Campanhas)" else "ponto amostral",
                       "vegan::specaccum, método aleatório, todas as ordens até 999, senão 999 sorteios", "vegan::specpool e poolaccum (Chao 2, Jackknife 1 e 2, Bootstrap), até 199 ordens",
                       "vegan::estimateR (Chao 1, ACE)", "iNEXT (números de Hill q = 0, 1, 2), 200 réplicas bootstrap",
                       "Shannon (log natural), Simpson, Berger-Parker, Margalef, Menhinick, alfa de Fisher, números de Hill",
                       "Pielou J', Simpson E, razão de Hill E2,1, Evar (Smith & Wilson 1996)",
                       "Jaccard e Bray-Curtis (vegan::vegdist), UPGMA com correlação cofenética, partição de Baselga (Sørensen)",
                       sprintf("%d registros secundários fora das análises", if (is.null(L$secundarios)) 0L else nrow(L$secundarios)),
                       basename(L$arquivo %||% "")), stringsAsFactors = FALSE)
}

exportar_relatorio_fauna <- function(L, R, saida) {
  pasta <- file.path(saida, "tabelas"); dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  for (g in names(R)) {
    G <- R[[g]]; X <- G$relatorio; if (is.null(X)) next
    folhas <- list(
      "Índices da área" = G$indices$area,
      "Índices por ponto" = G$indices$tabela,
      "Estimadores abundância" = X$estimadores_abundancia,
      "Por método" = X$por_metodo, "Método x espécie" = X$metodo_especies,
      "Esforço" = X$esforco,
      "Espécies de interesse" = X$interesse,
      "Campanhas" = X$sazonal$campanha$tabela, "Teste campanhas" = X$sazonal$campanha$teste,
      "Estações" = X$sazonal$estacao$tabela, "Teste estações" = X$sazonal$estacao$teste,
      "IPA (aves)" = X$ipa,
      "Métodos" = ficha_metodos(L, R))
    if (!is.null(L$secundarios) && nrow(L$secundarios)) folhas[["Dados secundários"]] <- L$secundarios[, intersect(c("ponto_original", "especie_completa", "familia", "grupo", "total", "dado", "observacao"), names(L$secundarios))]
    folhas <- folhas[!vapply(folhas, function(x) is.null(x) || !nrow(x), TRUE)]
    names(folhas) <- substr(names(folhas), 1, 31)
    writexl::write_xlsx(folhas, file.path(pasta, sprintf("09_%s_relatorio_fauna.xlsx", tag_grupo(g))))
  }
  invisible(TRUE)
}
