# O nome da unidade é criado por unidades_campanha(): ponto, ponto médio e o nome da campanha (por exemplo, "P1 · Chuva").
SEP_CAMP <- " · "
ponto_fisico <- function(unidade) { u <- as.character(unidade); i <- regexpr(SEP_CAMP, u, fixed = TRUE); ifelse(i > 0, substr(u, 1, i - 1), u) }
tem_campanha <- function(unidade) grepl(SEP_CAMP, as.character(unidade), fixed = TRUE)
campanha_da_unidade <- function(unidade) { u <- as.character(unidade); i <- regexpr(SEP_CAMP, u, fixed = TRUE); ifelse(i > 0, substring(u, i + nchar(SEP_CAMP)), NA_character_) }

plano_permutacao <- function(unidades, grupo) {
  ponto <- factor(ponto_fisico(unidades))
  g <- droplevels(factor(grupo))
  repetido <- anyDuplicated(ponto) > 0
  if (nlevels(g) < 2 || any(table(g) < 2))
    return(list(pulado = "São necessários dois ou mais grupos com pelo menos duas unidades em cada grupo."))
  if (repetido) {
    pres <- table(ponto, g) > 0
    informativo <- rowSums(pres) > 1
    if (sum(informativo) < 2 || any(colSums(pres[informativo, , drop = FALSE]) < 2))
      return(list(pulado = "O fator não varia em pelo menos dois pontos físicos para cada grupo. Visitas repetidas ao mesmo ponto não são réplicas independentes. Para comparar ambientes fixos, use uma unidade independente por ponto, com esforço comparável."))
    modelo <- stats::model.matrix(~ ponto + g)
    if (qr(modelo)$rank < ncol(modelo) || nrow(modelo) <= ncol(modelo))
      return(list(pulado = "O efeito do grupo não pode ser separado do efeito do ponto, ou não há graus de liberdade residuais."))
  }
  controle <- if (repetido) permute::how(nperm = 999, blocks = ponto) else permute::how(nperm = 999)
  list(grupo = g, ponto = ponto, blocos = repetido, controle = controle)
}

# A semente local mantém a rodada automática e a solicitada comparáveis sem afetar
# a sequência aleatória das outras análises ou os testes do usuário.
permutacoes_quadrante <- function(n, controle) {
  tinha_semente <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (tinha_semente) anterior <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (tinha_semente) assign(".Random.seed", anterior, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(20261119)
  suppressMessages(permute::shuffleSet(n, nset = 999, control = controle))
}

permanova_quadrante <- function(m, grupo) {
  if (length(grupo) != nrow(m))
    return(list(pulado = "Informe o grupo de cada unidade na aba de pontos ou no cadastro das campanhas."))
  selecionada <- rowSums(m) > 0 & !is.na(grupo) & nzchar(as.character(grupo))
  m <- m[selecionada, , drop = FALSE]
  if (nrow(m) < 6)
    return(list(pulado = sprintf("São necessárias ao menos seis unidades com registros e grupo informado. Há %d.", nrow(m))))
  plano <- plano_permutacao(rownames(m), grupo[selecionada])
  if (!is.null(plano$pulado)) return(plano)
  grupo <- plano$grupo; ponto <- plano$ponto
  d <- vegan::vegdist(sqrt(m), "bray")
  if (!any(d > 0)) return(list(pulado = "Todas as unidades têm as mesmas abundâncias. A composição não varia."))
  perm <- permutacoes_quadrante(nrow(m), plano$controle)
  ad <- if (plano$blocos) vegan::adonis2(d ~ ponto + grupo, by = "terms", permutations = perm)
        else vegan::adonis2(d ~ grupo, permutations = perm)
  linha <- if (plano$blocos) match("grupo", rownames(ad)) else 1L
  Fobs <- ad$F[linha]; p <- ad[["Pr(>F)"]][linha]; r2 <- ad$R2[linha]
  if (!is.finite(Fobs) || !is.finite(p) || ad$SumOfSqs[match("Residual", rownames(ad))] <= .Machine$double.eps * max(ad$SumOfSqs))
    return(list(pulado = "Não há variação residual suficiente para calcular um pseudo-F finito."))
  bd <- tryCatch(suppressWarnings(vegan::betadisper(d, grupo)), error = function(cond) NULL)
  pd <- if (is.null(bd)) NA_real_ else tryCatch(vegan::permutest(bd, permutations = perm)$tab[1, "Pr(>F)"], error = function(cond) NA_real_)
  coords <- NULL
  if (!is.null(bd) && !is.null(dim(bd$vectors)) && ncol(bd$vectors) >= 1) {
    coords <- data.frame(unidade = rownames(m), grupo = as.character(grupo), PCoA1 = bd$vectors[, 1],
                         PCoA2 = if (ncol(bd$vectors) >= 2) bd$vectors[, 2] else 0, row.names = NULL)
  }
  fontes <- if (plano$blocos) c("Ponto físico", "Grupo", "Resíduo", "Total") else c("Grupo", "Resíduo", "Total")
  tabela <- data.frame(fonte = fontes, gl = ad$Df, soma_quadrados = ad$SumOfSqs, R2 = ad$R2, F = ad$F, p = ad[["Pr(>F)"]])
  if (plano$blocos) tabela$p[1] <- NA_real_  # o ponto é controle do delineamento, não o efeito testado
  esforco <- if (plano$blocos) "O efeito do grupo foi calculado após controlar o ponto físico. A PERMANOVA e o teste de dispersão usam permutações dentro de cada ponto." else "As unidades físicas distintas foram permutadas livremente."
  f <- function(x, casas = 3) formatC(x, format = "f", digits = casas, decimal.mark = ",")
  dispersao <- if (!is.finite(pd)) "A dispersão não pôde ser testada." else sprintf("Teste de dispersão: p = %s.", f(pd))
  rot <- paste(sprintf("%s (%d)", names(table(grupo)), as.integer(table(grupo))), collapse = ", ")
  texto <- list(mede = "A PERMANOVA compara a composição dos grupos pela dissimilaridade de Bray–Curtis calculada sobre a raiz quadrada das abundâncias.",
                resultado = sprintf("Grupos: %s. R² = %s; pseudo-F = %s; p = %s. %s", rot, f(r2), f(Fobs), f(p), dispersao),
                interpretacao = if (p >= .05) "Os dados não mostraram evidência de diferença entre os grupos." else if (!is.finite(pd) || pd < .05) "Houve diferença na estrutura multivariada. A dispersão precisa ser considerada na interpretação." else "Houve evidência de diferença na composição entre os grupos.",
                limites = sprintf("%s Foram avaliados %d rearranjos, além da configuração observada. R² é a fração da soma de quadrados total atribuída ao grupo.", esforco, nrow(perm)))
  list(R2 = r2, F = Fobs, p = p, p_dispersao = pd, grupos = rot, blocos = plano$blocos,
       n_perm = nrow(perm), texto = texto, tabela = tabela, coordenadas = coords)
}
