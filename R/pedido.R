# Quadrante: análise sob pedido (19/11/2026, pedido do usuário: "eu peço uma análise e o sistema busca no R o pacote,
# o ggplot e confere todos os pressupostos antes de rodar").
# Uso: Rscript pedido.R "<pasta do levantamento>" <analise> <grupo> [fator] [variavel]
#   analise  = id do R/pedidos_catalogo.json (nmds, permanova, anosim, simper, kruskal, anova, correlacao_esforco, mantel, pca, quiquadrado, indval)
#   fator    = fitofisionomia | grupo_amostral | campanha | estacao (quando a análise compara grupos)
#   variavel = riqueza | abundancia (kruskal e anova)
# Confere cada pressuposto nos dados deste levantamento: "ok", "atenção" (roda e avisa) ou "falha" (não roda e diz por quê).
# Grava em pedidos/: <id>.json (pressupostos, texto, tabela), <id>.png e <id>.jpg (figura ggplot no estilo do painel)
# e acrescenta a entrada em pedidos/indice.json. Usa os resultados já calculados (resultados.rds); não muda nada deles.

local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
suppressPackageStartupMessages({ library(ggplot2); library(jsonlite); library(vegan) })
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && !is.list(a) && is.na(a))) b else a
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")
fp <- function(p) if (is.na(p)) "não calculado" else if (p < 0.001) "< 0,001" else paste("=", fmt(p, 3))
source(file.path(aqui, "saida.R"), encoding = "UTF-8"); source(file.path(aqui, "contexto.R"), encoding = "UTF-8")
source(file.path(aqui, "desenho.R"), encoding = "UTF-8")
source(file.path(aqui, "conferir.R"), encoding = "UTF-8")
sem_acento <- function(x) tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT"))
set.seed(20261119)
options(OutDec = ",")   # vírgula decimal nos eixos das figuras

a <- commandArgs(TRUE)
saida <- a[1]; analise <- a[2]; grupo <- a[3]; fator <- if (length(a) >= 4 && nzchar(a[4]) && a[4] != "-") a[4] else ""; variavel <- if (length(a) >= 5 && nzchar(a[5])) a[5] else "riqueza"
if (analise == "quasipoisson") variavel <- "abundancia"
if (analise == "regressao_gradiente" && (!grepl("^amb_[0-9a-f]{32}$", fator) || !variavel %in% c("riqueza","abundancia")))
  stop("Selecione uma medida ambiental e uma resposta no painel.")
cat_ <- fromJSON(file.path(aqui, "pedidos_catalogo.json"), simplifyVector = FALSE)$analises
info <- Filter(function(x) identical(x$id, analise), cat_)[[1]]
if (is.null(info)) stop("Análise desconhecida: ", analise)
cat("@@ 5 Lendo os resultados do levantamento\n")

x <- readRDS(file.path(saida, "resultados.rds"))
m_total <- x$objs[[grupo]]$m
resumo <- x$R[[grupo]]
if (is.null(m_total)) {
  matriz <- resumo$matriz
  if (is.null(matriz$valores)) stop("Grupo sem matriz de espécies: ", grupo)
  m_total <- matriz$valores
  dimnames(m_total) <- list(matriz$pontos, matriz$especies)
}
m <- m_total[rowSums(m_total) > 0, colSums(m_total) > 0, drop = FALSE]
js <- readLines(file.path(saida, "dados.js"), encoding = "UTF-8", warn = FALSE)
H <- fromJSON(sub(";\\s*$", "", sub("^window\\.HERP = ", "", paste(js[-1], collapse = "\n"))), simplifyVector = TRUE)
P <- H$pontos; ctx <- ler_contexto(saida)
e <- ler_estilo(file.path(saida, "estilo.json")); registrar_fontes(e); k <- cores_tema(e); pal <- e$paleta
# Nas respostas univariadas, zero observado é uma medição válida.
sem_confirmacao <- character()
if (analise %in% c("anova", "kruskal", "quasipoisson", "correlacao_esforco", "regressao_gradiente")) {
  esforco <- suppressWarnings(as.numeric(P$esforco[match(rownames(m_total), P$ponto)]))
  tem_visita <- rowSums(m_total) > 0 | rownames(m_total) %in% resumo$pontos_zero_explicito
  if (length(esforco)) tem_visita <- tem_visita | (is.finite(esforco) & esforco > 0)
  sem_confirmacao <- rownames(m_total)[!tem_visita]
  m <- m_total[tem_visita, , drop = FALSE]
}
un <- rownames(m); n <- length(un); multi <- any(tem_campanha(un))
repetido <- anyDuplicated(ponto_fisico(un)) > 0

# ---- fator de cada unidade amostral
nomes_fator <- c(fitofisionomia = "Fitofisionomia", grupo_amostral = "Grupo amostral", campanha = "Campanha", estacao = "Estação")
fator_de <- function(f) {
  if (!nzchar(f) || is.null(P)) return(NULL)
  i <- match(un, P$ponto)
  v <- switch(f,
    fitofisionomia = P$fitofisionomia[i], grupo_amostral = P$grupo_amostral[i],
    campanha = if (multi) campanha_da_unidade(un) else NULL,
    estacao = { cs <- ctx$campanhas; if (is.null(cs)) NULL else {
      cp <- if (!is.null(P$campanha)) P$campanha[i] else rep(NA, n); kk <- campanha_ctx(cs, cp); unname(ESTACOES[as.character(cs$estacao[kk])]) } },
    NULL)
  if (is.null(v)) return(NULL)
  v[is.na(v) | !nzchar(v)] <- NA
  setNames(v, un)
}
g <- fator_de(fator)

# ---- pressupostos
PR <- list()
pr <- function(item, nivel, detalhe) PR[[length(PR) + 1]] <<- list(item = item, nivel = nivel, detalhe = detalhe)
falhou <- function() any(vapply(PR, function(p) p$nivel == "falha", TRUE))
pede_fator <- function(min_por_grupo = 2) {
  if (!nzchar(fator)) { pr("Fator para comparar", "falha", "Escolha o que comparar: fitofisionomia, grupo amostral, campanha ou estação."); return(invisible()) }
  if (is.null(g) || all(is.na(g))) { pr("Fator para comparar", "falha", sprintf("O levantamento não tem %s para as unidades amostrais (preencha na aba de pontos ou no contexto do campo).", tolower(nomes_fator[fator]))); return(invisible()) }
  if (any(is.na(g))) pr("Unidades sem grupo", "atenção", sprintf("%d unidades sem %s ficam de fora: %s.", sum(is.na(g)), tolower(nomes_fator[fator]), paste(names(g)[is.na(g)], collapse = ", ")))
  tb <- table(g[!is.na(g)])
  if (length(tb) < 2) { pr("Dois ou mais grupos", "falha", sprintf("Só há um grupo (%s).", names(tb)[1])); return(invisible()) }
  pr("Dois ou mais grupos", "ok", paste(sprintf("%s: %d unidade%s", names(tb), as.integer(tb), ifelse(tb > 1, "s", "")), collapse = "; "))
  if (any(tb < min_por_grupo)) pr(sprintf("%d ou mais unidades por grupo", min_por_grupo), "falha", sprintf("Grupos com poucas unidades: %s. Junte grupos parecidos ou compare outro fator.", paste(names(tb)[tb < min_por_grupo], collapse = ", ")))
  else pr(sprintf("%d ou mais unidades por grupo", min_por_grupo), "ok", "Atendido.")
}
falha_repeticao <- function() pr("Independência entre unidades", "falha", "A análise exige unidades independentes. O mesmo ponto físico foi visitado mais de uma vez.")

res <- list(id = analise, nome = info$nome, pacote = info$pacote, funcao = info$funcao, grupo = grupo, fator = fator,
            fator_nome = unname(nomes_fator[fator]) %||% "", variavel = variavel, feito_em = format(Sys.time(), "%d/%m/%Y %H:%M"))
fig <- NULL; tabela <- NULL; texto <- NULL
cor_g <- function(v) { lv <- sort(unique(v[!is.na(v)])); setNames(rep(pal, length.out = length(lv)), lv) }
tt <- function(titulo, xx = NULL, yy = NULL) list(labs(title = if (isTRUE(e$mostrar_titulo)) titulo else NULL, x = xx, y = yy), tema_gg(e), coord_cartesian(clip = "off"))
folga <- list(scale_x_continuous(expand = expansion(mult = .18)), scale_y_continuous(expand = expansion(mult = .12)))

cat("@@ 30 Conferindo os pressupostos\n")
pac1 <- strsplit(info$pacote, " ")[[1]][1]
if (!requireNamespace(pac1, quietly = TRUE)) {
  pr(sprintf("Pacote %s instalado", info$pacote), "falha", sprintf("O pacote %s não está instalado neste computador. Instalar precisa da sua aprovação (guarda de instalação).", info$pacote))
} else pr(sprintf("Pacote %s instalado", info$pacote), "ok", sprintf("%s %s", strsplit(info$pacote, " ")[[1]][1], as.character(utils::packageVersion(strsplit(info$pacote, " ")[[1]][1]))))

if (length(sem_confirmacao)) pr("Unidades sem confirmação de visita", "atenção",
  sprintf("Excluídas %s. Para incluir um zero, confirme o esforço ou registre a contagem zero na planilha e recalcule.", paste(sem_confirmacao, collapse = ", ")))

if (analise == "nmds") {
  if (n < 6) pr("6 ou mais unidades amostrais", "falha", sprintf("Há %d unidades. Com menos de 6 o NMDS não tem restrição suficiente e o mapa sai arbitrário (decisão de 17/11/2026).", n)) else pr("6 ou mais unidades amostrais", "ok", sprintf("%d unidades.", n))
  if (ncol(m) < 3) pr("3 ou mais espécies", "falha", sprintf("Só %d espécies.", ncol(m))) else pr("3 ou mais espécies", "ok", sprintf("%d espécies.", ncol(m)))
  if (n >= 2 && !any(vegan::vegdist(sqrt(m), "bray") > 0)) pr("Variação entre unidades", "falha", "As abundâncias são idênticas entre as unidades.")
  if (!falhou()) {
    cat("@@ 50 Rodando vegan::metaMDS\n")
    mds <- suppressMessages(vegan::metaMDS(sqrt(m), distance = "bray", k = 2, trymax = 100, autotransform = FALSE, trace = 0))
    s <- mds$stress
    if (s < 0.001) pr("Stress quase zero", "atenção", "Stress perto de zero costuma indicar dados de menos ou unidades quase idênticas; o mapa pode exagerar a separação.")
    pr("Stress abaixo de 0,2", if (s < 0.1) "ok" else if (s < 0.2) "atenção" else "falha", sprintf("Stress = %s (%s).", fmt(s, 3), if (s < 0.1) "boa representação" else if (s < 0.2) "representação aceitável; interprete só os grupos claros" else "o mapa não representa bem as distâncias; não interprete"))
    sc <- as.data.frame(vegan::scores(mds, display = "sites")); sc$unidade <- rownames(sc); sc$grupo <- if (!is.null(g)) g[rownames(sc)] else "Unidades"
    tabela <- data.frame(unidade = sc$unidade, NMDS1 = round(sc$NMDS1, 3), NMDS2 = round(sc$NMDS2, 3), grupo = sc$grupo, stringsAsFactors = FALSE)
    fig <- ggplot(sc, aes(NMDS1, NMDS2, colour = grupo)) + geom_hline(yintercept = 0, colour = k$grade) + geom_vline(xintercept = 0, colour = k$grade) +
      geom_point(size = e$ponto * 1.4) + geom_text(aes(label = unidade, hjust = ifelse(NMDS1 > 0, -.35, 1.35)), size = e$fonte / 3.2, family = e$familia, show.legend = FALSE) +
      scale_colour_manual(values = cor_g(sc$grupo), name = res$fator_nome) + tt("NMDS (Bray-Curtis)", "NMDS 1", "NMDS 2") + folga +
      annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -.6, label = paste("stress =", fmt(s, 3)), family = e$familia, size = e$fonte / 3.4, colour = k$tinta2)
    if (is.null(g)) fig <- fig + theme(legend.position = "none")
    texto <- list(mede = "Coloca as unidades amostrais num mapa: unidades próximas têm espécies e abundâncias parecidas (Bray–Curtis sobre a raiz quadrada das contagens).",
      resultado = sprintf("%d unidades e %d espécies; stress = %s.", n, ncol(m), fmt(s, 3)),
      interpretacao = "Grupos de pontos separados no mapa indicam comunidades diferentes. Para saber se a diferença é real, rode a PERMANOVA com o mesmo fator.",
      limites = "O NMDS só descreve; não testa. Contagens estimadas (coros) pesam muito no Bray-Curtis.")
  }
}

if (analise %in% c("permanova", "anosim", "simper", "indval")) {
  pede_fator(if (analise == "indval") 3 else 2)
  if (!falhou()) {
    ok <- !is.na(g); mm <- m[ok, , drop = FALSE]; gg_ <- droplevels(factor(g[ok]))
    mm <- mm[, colSums(mm) > 0, drop = FALSE]
    if (analise %in% c("anosim", "indval")) {
      plano <- plano_permutacao(rownames(mm), gg_)
      if (!is.null(plano$pulado)) pr("Delineamento da comparação", "falha", plano$pulado)
      else if (plano$blocos) pr("Visitas repetidas", "ok", "Permutações restritas ao ponto físico.")
      if (analise == "anosim" && length(unique(as.vector(vegan::vegdist(mm, "bray")))) < 2)
        pr("Variação nas dissimilaridades", "falha", "As distâncias são constantes entre as unidades.")
    }
  }
  if (!falhou() && analise == "permanova") {
    cat("@@ 50 Rodando vegan::adonis2 e betadisper\n")
    resultado <- permanova_quadrante(mm, gg_)
    if (!is.null(resultado$pulado)) pr("Delineamento da comparação", "falha", resultado$pulado)
    else {
      pr("Delineamento da comparação", "ok", resultado$texto$limites)
      pb <- resultado$p_dispersao
      pr("Dispersão entre grupos", if (!is.finite(pb) || pb < .05) "atenção" else "ok",
         if (!is.finite(pb)) "O teste de dispersão não foi estimável." else sprintf("Teste de dispersão: p %s.", fp(pb)))
      tabela <- resultado$tabela
      tabela$fonte[tabela$fonte == "Grupo"] <- res$fator_nome
      texto <- resultado$texto; co <- resultado$coordenadas
      if (!is.null(co)) {
        ce <- aggregate(cbind(PCoA1, PCoA2) ~ grupo, co, mean)
        fig <- ggplot(co, aes(PCoA1, PCoA2, colour = grupo)) +
          geom_segment(data = merge(co, ce, by = "grupo", suffixes = c("", ".c")), aes(xend = PCoA1.c, yend = PCoA2.c), alpha = .45, linewidth = .5 * e$linha, show.legend = FALSE) +
          geom_point(size = e$ponto * 1.3) + geom_point(data = ce, shape = 4, size = e$ponto * 2.2, stroke = 1.3, show.legend = FALSE) +
          geom_text(aes(label = unidade, hjust = ifelse(PCoA1 > 0, -.35, 1.35)), size = e$fonte / 3.4, family = e$familia, show.legend = FALSE) +
          scale_colour_manual(values = cor_g(co$grupo), name = res$fator_nome) + tt(paste("Composição por", tolower(res$fator_nome)), "PCoA 1", "PCoA 2") + folga
      }
    }
  }
  if (!falhou() && analise == "anosim") {
    cat("@@ 50 Rodando vegan::anosim\n")
    an <- vegan::anosim(mm, gg_, distance = "bray", permutations = permutacoes_quadrante(nrow(mm), plano$controle))
    tabela <- data.frame(estatistica_R = round(an$statistic, 3), p = signif(an$signif, 3), permutacoes = an$permutations)
    dd <- data.frame(classe = an$class.vec, rank = an$dis.rank)
    fig <- ggplot(dd, aes(classe, rank, fill = classe)) + geom_boxplot(width = .55, colour = k$tinta2, outlier.shape = NA) + geom_jitter(width = .12, size = e$ponto * .7, colour = k$tinta2) +
      scale_fill_manual(values = c(Between = "#9a9384", setNames(rep(pal, 10)[seq_along(levels(gg_))], levels(gg_))), guide = "none") +
      scale_x_discrete(labels = function(z) ifelse(z == "Between", "Entre grupos", z)) + tt("ANOSIM: postos das distâncias", NULL, "Posto da dissimilaridade")
    texto <- list(mede = "Compara as distâncias dentro dos grupos com as distâncias entre grupos (R vai de -1 a 1).",
      resultado = sprintf("R = %s; p %s.", fmt(an$statistic, 3), fp(an$signif)),
      interpretacao = if (an$statistic > .5 && an$signif < .05) "Grupos bem separados." else if (an$signif < .05) "Grupos diferentes, mas com sobreposição." else "Sem separação clara entre os grupos.",
      limites = "O ANOSIM é sensível a dispersões diferentes entre grupos; prefira a PERMANOVA com betadisper para o laudo.")
  }
  if (!falhou() && analise == "simper") {
    cat("@@ 50 Rodando vegan::simper\n")
    sp <- summary(vegan::simper(mm, gg_, permutations = 0))
    tabela <- do.call(rbind, lapply(names(sp), function(nm) { s <- sp[[nm]]; s <- head(s[order(-s$average), ], 10)
      data.frame(par = gsub("_", " x ", nm), especie = rownames(s), contribuicao = round(s$average, 4), acumulado = round(s$cumsum, 3), media_a = round(s$ava, 2), media_b = round(s$avb, 2), stringsAsFactors = FALSE) }))
    t1 <- tabela[tabela$par == tabela$par[1], ]; t1$especie <- factor(t1$especie, rev(t1$especie))
    fig <- ggplot(t1, aes(especie, contribuicao)) + geom_col(fill = pal[1], width = .7) + coord_flip() +
      geom_text(aes(label = paste0(fmt(100 * acumulado, 0), "%")), hjust = -.15, size = e$fonte / 3.4, family = e$familia, colour = k$tinta2) +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) + tt(paste("Espécies que mais diferenciam", t1$par[1]), NULL, "Contribuição média para a dissimilaridade") +
      theme(axis.text.y = element_text(face = "italic"))
    texto <- list(mede = "Decompõe a dissimilaridade de Bray-Curtis entre dois grupos na contribuição de cada espécie.",
      resultado = sprintf("No par %s, as 3 espécies que mais contribuem são %s (juntas, %s da diferença).", t1$par[1], paste(head(as.character(t1$especie), 3), collapse = ", "), paste0(fmt(100 * t1$acumulado[min(3, nrow(t1))], 0), "%")),
      interpretacao = "Espécies no topo são as que mudaram de abundância entre os grupos; confira se são as mais abundantes (o SIMPER favorece espécies comuns).",
      limites = "Não é um teste: diz quem contribui, não se a diferença é significativa (para isso, PERMANOVA). Percentuais na figura são acumulados.")
  }
  if (!falhou() && analise == "indval") {
    cat("@@ 50 Rodando indicspecies::multipatt\n")
    mp <- indicspecies::multipatt(mm, gg_, func = "IndVal.g", control = plano$controle)
    s <- mp$sign; s <- s[!is.na(s$p.value), ]; grs <- levels(gg_)
    tabela <- data.frame(especie = rownames(s), grupo = vapply(seq_len(nrow(s)), function(i) paste(grs[unlist(s[i, paste0("s.", grs)]) == 1], collapse = " + "), ""), IndVal = round(s$stat, 3), p = signif(s$p.value, 3), stringsAsFactors = FALSE)
    tabela$p_holm <- stats::p.adjust(s$p.value, method = "holm")
    tabela <- tabela[order(tabela$p_holm, -tabela$IndVal), ]
    texto <- list(mede = "Acha as espécies típicas de cada grupo (fiéis e frequentes ali).", resultado = sprintf("%d espécies com p ajustado por Holm < 0,05.", sum(tabela$p_holm < .05)),
      interpretacao = "Espécies indicadoras ajudam a monitorar cada ambiente.", limites = "Com poucas unidades por grupo, nenhuma espécie alcança p baixo.")
  }
}

if (analise %in% c("kruskal", "anova")) {
  pede_fator(2)
  if (!falhou()) {
    ok <- !is.na(g); pontos <- factor(ponto_fisico(un[ok])); grupos <- factor(g[ok])
    pareado <- anyDuplicated(pontos) > 0
    y0 <- if (variavel == "abundancia") rowSums(m) else rowSums(m > 0)
    if (length(unique(y0[ok])) < 2) pr("Variação na resposta", "falha", "A resposta é constante nas unidades com grupo informado.")
    if (analise == "anova" && all(tapply(y0[ok], grupos, function(v) length(unique(v)) < 2)))
      pr("Variação residual", "falha", "Nenhum grupo apresenta variação interna. O erro residual da ANOVA não é estimável.")
    if (pareado && (analise == "anova" || !all(table(pontos, grupos) == 1)))
      pr("Delineamento pareado", "falha", "Para visitas repetidas, o pedido de comparação por postos admite uma medição por nível do fator em cada ponto. A ANOVA disponível é para pontos independentes.")
    if (pareado && !falhou()) {
      if (!any(tapply(y0[ok], pontos, function(x) length(unique(x)) > 1)))
        pr("Variação dentro de pontos", "falha", "A resposta não muda entre as condições dentro de nenhum ponto.")
      else pr("Delineamento pareado", "ok", "Os níveis do fator serão comparados dentro de cada ponto.")
    }
  }
  if (!falhou()) {
    y <- if (variavel == "abundancia") rowSums(m) else rowSums(m > 0); nomev <- if (variavel == "abundancia") "Abundância (indivíduos)" else "Riqueza (espécies)"
    ok <- !is.na(g); d <- data.frame(unidade = un[ok], grupo = g[ok], y = y[ok], stringsAsFactors = FALSE)
    tb <- table(d$grupo); if (any(tb < 5)) pr("5 ou mais unidades por grupo", "atenção", "Grupos pequenos: o teste tem pouco poder; p alto não prova igualdade.")
    if (analise == "anova") {
      fit <- stats::aov(y ~ grupo, d); rs <- stats::residuals(fit)
      sh <- if (length(rs) >= 3 && length(rs) <= 5000 && length(unique(rs)) > 2) stats::shapiro.test(rs)$p.value else NA
      bt <- tryCatch(stats::bartlett.test(y ~ grupo, d)$p.value, error = function(x) NA)
      pr("Resíduos normais (Shapiro-Wilk)", if (is.na(sh)) "atenção" else if (sh < .05) "atenção" else "ok", if (is.na(sh)) "Não deu para testar (poucos dados)." else sprintf("p %s%s", fp(sh), if (sh < .05) ": há evidência de desvio de normalidade. Examine o gráfico dos resíduos." else "."))
      pr("Variâncias iguais (Bartlett)", if (is.na(bt)) "atenção" else if (bt < .05) "atenção" else "ok", if (is.na(bt)) "Não deu para testar (grupo com uma unidade ou sem variação)." else sprintf("p %s%s", fp(bt), if (bt < .05) ": há evidência de heterogeneidade. Examine a variância por grupo." else "."))
      sm <- summary(fit)[[1]]; pv <- sm$`Pr(>F)`[1]
      tabela <- data.frame(fonte = c(res$fator_nome, "Resíduo"), gl = sm$Df, soma_quadrados = round(sm$`Sum Sq`, 3), quadrado_medio = round(sm$`Mean Sq`, 3), F = round(sm$`F value`, 3), p = signif(sm$`Pr(>F)`, 3))
      rt <- sprintf("F = %s; p %s.", fmt(sm$`F value`[1], 2), fp(pv))
    } else {
      dois <- nlevels(grupos) == 2
      if (pareado) {
        tab <- tapply(d$y, list(pontos, grupos), sum)
        teste <- if (dois) stats::wilcox.test(tab[, 1], tab[, 2], paired = TRUE)
                 else stats::friedman.test(tab)
      } else {
        teste <- if (dois) stats::wilcox.test(y ~ grupo, d) else stats::kruskal.test(y ~ factor(grupo), d)
      }
      pv <- teste$p.value; res$funcao <- if (pareado && dois) "wilcox.test (postos sinalizados, pareado)" else if (pareado) "friedman.test" else if (dois) "wilcox.test (Mann–Whitney)" else "kruskal.test"
      tabela <- data.frame(teste = teste$method, estatistica = round(unname(teste$statistic), 3), gl = unname(teste$parameter %||% NA), p = signif(pv, 3))
      rt <- sprintf("%s = %s; p %s.", names(teste$statistic), fmt(teste$statistic, 2), fp(pv))
    }
    md <- aggregate(y ~ grupo, d, function(v) c(media = mean(v), mediana = stats::median(v), n = length(v)))
    md <- data.frame(grupo = md$grupo, md$y); tabela <- list(teste = tabela, por_grupo = transform(md, media = round(media, 2)))
    fig <- ggplot(d, aes(grupo, y, fill = grupo)) + geom_boxplot(width = .55, alpha = .55, colour = k$tinta2, outlier.shape = NA) +
      geom_jitter(width = .1, height = 0, size = e$ponto * .9, colour = k$tinta) + scale_fill_manual(values = cor_g(d$grupo), guide = "none") +
      tt(paste(nomev, "por", tolower(res$fator_nome)), NULL, nomev)
    texto <- list(mede = sprintf("Compara a %s de cada unidade amostral entre os grupos de %s.", tolower(sub(" \\(.*", "", nomev)), tolower(res$fator_nome)),
      resultado = paste(rt, paste(sprintf("%s: média %s (n = %d)", md$grupo, fmt(md$media, 1), as.integer(md$n)), collapse = "; ")),
      interpretacao = if (!is.finite(pv)) "O teste não produziu um valor de p interpretável." else if (pv < .05) "Há diferença entre os grupos." else "Sem evidência de diferença entre os grupos.",
      limites = if (analise == "anova") "Com resíduos assimétricos ou variâncias desiguais, confira a distribuição da resposta e a adequação do modelo antes de interpretar o teste F." else "O teste não paramétrico compara postos; não diz o tamanho da diferença.")
  }
}

if (analise == "regressao_gradiente") {
  indice <- match(fator, H$gradientes$chave)
  if (is.na(indice)) {
    pr("Variável ambiental", "falha", "A medida selecionada não está nos dados deste levantamento. Recalcule a planilha e escolha o preditor.")
  } else {
    nome_x <- H$gradientes$nome[indice]; res$fator_nome <- nome_x
    valores <- P[[paste0("amb.", nome_x)]]
    if (is.null(valores)) pr("Variável ambiental", "falha", "A coluna selecionada está ausente do cadastro dos pontos.")
  }
  if (!falhou()) {
    medidas <- split(valores[P$ponto %in% un], P$ponto[P$ponto %in% un])
    ambiguos <- names(medidas)[vapply(medidas, function(v) length(unique(v[is.finite(v)])) > 1L, logical(1))]
    if (length(ambiguos)) pr("Medida por unidade", "falha", paste("Valores conflitantes na mesma unidade:", paste(ambiguos, collapse = ", ")))
    por_unidade <- vapply(medidas, function(v) as.numeric(v[is.finite(v)][1L]), numeric(1))
    xs <- unname(por_unidade[un])
    y <- if (variavel == "riqueza") rowSums(m > 0) else rowSums(m)
    dados <- data.frame(unidade = un, x = xs, y = as.numeric(y))
    incluir <- is.finite(xs)
    if (any(!incluir)) pr("Medidas ausentes", "atenção", paste("Unidades sem a medida:", paste(un[!incluir], collapse = ", ")))
    dados <- dados[incluir, , drop = FALSE]
    if (nrow(dados) < 3L || length(unique(dados$x)) < 2L || length(unique(dados$y)) < 2L)
      pr("Dados para estimar a inclinação", "falha", "São necessárias três unidades completas ou mais, com variação no preditor e na resposta.")
    if (anyDuplicated(ponto_fisico(dados$unidade))) falha_repeticao()
  }
  if (!falhou()) {
    fit <- stats::lm(y ~ x, data = dados)
    if (fit$rank < 2L || sum(residuals(fit)^2) <= .Machine$double.eps * sum((dados$y - mean(dados$y))^2))
      pr("Variância residual", "falha", "A inclinação ou a incerteza residual não é estimável com precisão neste conjunto.")
  }
  if (!falhou()) {
    sm <- summary(fit); co <- coef(sm); ic <- stats::confint(fit)
    nome_y <- if (variavel == "riqueza") "Riqueza (espécies)" else "Abundância (indivíduos)"
    rr <- residuals(fit)
    if (length(rr) <= 5000 && diff(range(rr)) > 0) {
      sw <- stats::shapiro.test(rr)$p.value
      pr("Normalidade dos resíduos", if (sw < .05) "atenção" else "ok", sprintf("Shapiro–Wilk p %s. Avalie também a distribuição dos resíduos.", fp(sw)))
    }
    pr("Delineamento e forma da relação", "atenção", "Confira esforço comparável, independência espacial, linearidade e dispersão dos resíduos ao longo do gradiente.")
    tabela <- list(coeficientes = data.frame(termo = c("Intercepto", nome_x), estimativa = co[,1], erro_padrao = co[,2], IC95_inf = ic[,1], IC95_sup = ic[,2], t = co[,3], p = co[,4], row.names = NULL),
      ajuste = data.frame(n = nrow(dados), gl_residuais = fit$df.residual, R2 = sm$r.squared, R2_ajustado = sm$adj.r.squared),
      diagnostico = data.frame(unidade = dados$unidade, preditor = dados$x, observado = dados$y, ajustado = fitted(fit), residuo = rr, distancia_Cook = unname(stats::cooks.distance(fit)), row.names = NULL))
    fig <- ggplot(dados, aes(x, y)) + geom_point(colour = pal[1]) +
      geom_smooth(method = "lm", formula = y ~ x, level = .95, colour = pal[2], fill = pal[2], alpha = .15) +
      tt("Regressão linear", nome_x, nome_y)
    texto <- list(mede = paste("Associação linear entre", nome_x, "e", tolower(nome_y), "nos pontos amostrados."),
      resultado = sprintf("Inclinação = %s (IC 95%% de %s a %s), p %s. R² = %s em %d unidades.", fmt(co[2,1], 4), fmt(ic[2,1], 4), fmt(ic[2,2], 4), fp(co[2,4]), fmt(sm$r.squared, 4), nrow(dados)),
      interpretacao = "A inclinação expressa a variação média estimada da resposta por unidade do preditor na faixa observada.",
      limites = "A inferência depende do desenho e da adequação do modelo linear às contagens. Consulte a tabela de resíduos e influência antes de interpretar os intervalos. Uma associação observacional não demonstra causalidade.")
  }
}

if (analise == "quasipoisson") {
  pede_fator(2)
  if (!falhou()) {
    ok <- !is.na(g)
    dados <- data.frame(contagem = rowSums(m)[ok], tratamento = droplevels(factor(g[ok])))
    if (anyDuplicated(ponto_fisico(un[ok]))) falha_repeticao()
    if (any(tapply(dados$contagem, dados$tratamento, sum) == 0))
      pr("Contagens por grupo", "falha", "Há grupo sem detecções em todas as suas unidades. O contraste logarítmico com esse grupo não tem estimativa finita.")
    horas <- if (is.null(P$esforco)) rep(NA_real_, nrow(dados)) else suppressWarnings(as.numeric(P$esforco[match(un[ok], P$ponto)]))
    usa_horas <- any(!is.na(horas))
    if (usa_horas && any(!is.finite(horas) | horas <= 0))
      pr("Exposição", "falha", "Informe esforço positivo e na mesma unidade para todas as observações incluídas.")
    if (!usa_horas) pr("Exposição", "atenção", "O modelo compara contagens por unidade amostral. A interpretação requer exposições comparáveis entre unidades.")
    if (usa_horas && !falhou()) pr("Exposição", "ok", "O logaritmo das horas de esforço entra no modelo como offset.")
    dados$exposicao <- if (usa_horas) horas else 1
  }
  if (!falhou()) {
    fit <- stats::glm(contagem ~ tratamento + offset(log(exposicao)), data = dados, family = stats::quasipoisson(link = "log"))
    sm <- summary(fit)
    if (!isTRUE(fit$converged) || !is.finite(sm$dispersion) || sm$dispersion <= sqrt(.Machine$double.eps))
      pr("Ajuste e dispersão residual", "falha", "O ajuste ou sua dispersão residual não sustenta erros padrão para os contrastes.")
  }
  if (!falhou()) {
    co <- stats::coef(sm); niveis <- levels(dados$tratamento)
    ref <- niveis[1]; unidade <- if (usa_horas) "hora" else "unidade amostral"
    tabela <- list(coeficientes = data.frame(termo = c(paste("Intercepto", ref), paste(niveis[-1], "/", ref)),
      estimativa = co[, 1], erro_padrao = co[, 2], t = co[, 3], p = co[, 4], row.names = NULL),
      ajuste = data.frame(n = nrow(dados), gl_residuais = fit$df.residual, dispersao = sm$dispersion, referencia = ref, denominador = unidade))
    dados$taxa <- dados$contagem / dados$exposicao
    pred <- data.frame(tratamento = factor(niveis, levels = niveis), exposicao = 1)
    pred$taxa <- stats::predict(fit, pred, type = "response")
    fig <- ggplot(dados, aes(tratamento, taxa)) + geom_point(position = position_jitter(width = .09, height = 0, seed = 1), colour = pal[1]) +
      geom_point(data = pred, shape = 18, colour = pal[2], size = e$ponto * 2) + tt("Contagens por grupo", res$fator_nome, paste("Registros por", unidade))
    texto <- list(mede = paste("Regressão quase-Poisson com ligação log para as contagens por", unidade, "em cada grupo."),
      resultado = sprintf("%d unidades. Grupo de referência %s. Dispersão de Pearson = %s.", nrow(dados), ref, fmt(sm$dispersion, 3)),
      interpretacao = "Exponenciar um coeficiente de contraste dá a razão entre a média do grupo e a da referência, para a mesma exposição. Os losangos mostram as médias ajustadas.",
      limites = "O modelo admite um fator e unidades independentes. Inspecione resíduos, observações influentes e possíveis diferenças de detectabilidade. Os valores de p são por contraste, sem ajuste de multiplicidade.")
  }
}

if (analise == "correlacao_esforco") {
  if (repetido) falha_repeticao()
  ef <- if (!is.null(P$esforco)) suppressWarnings(as.numeric(P$esforco[match(un, P$ponto)])) else rep(NA_real_, n)
  ef[!is.finite(ef) | ef <= 0] <- NA_real_
  if (length(unique(rowSums(m > 0)[!is.na(ef)])) < 2) pr("Variação na riqueza", "falha", "A riqueza precisa variar entre as unidades com esforço informado.")
  if (sum(!is.na(ef)) < 4) pr("Esforço em 4 ou mais unidades", "falha", "O esforço não foi informado. Preencha a coluna Esforço (h) na aba de pontos ou o contexto do campo (observadores e horas por ponto) e recalcule.")
  else if (length(unique(ef[!is.na(ef)])) < 2) pr("Esforço diferente entre as unidades", "falha", "Todas as unidades tiveram o mesmo esforço: não há o que correlacionar (e isso é bom: os pontos se comparam direto).")
  else pr("Esforço informado", "ok", sprintf("%d unidades com esforço.", sum(!is.na(ef))))
  if (!falhou()) {
    d <- data.frame(unidade = un, esforco = ef, riqueza = rowSums(m > 0))[!is.na(ef), ]
    ct <- suppressWarnings(stats::cor.test(d$esforco, d$riqueza, method = "spearman", exact = FALSE))
    tabela <- data.frame(rho = round(ct$estimate, 3), p = signif(ct$p.value, 3), unidades = nrow(d))
    fig <- ggplot(d, aes(esforco, riqueza)) + geom_smooth(method = "lm", formula = y ~ x, colour = pal[2], fill = pal[2], alpha = .15, linewidth = e$linha) +
      geom_point(size = e$ponto * 1.3, colour = pal[1]) + geom_text(aes(label = unidade), vjust = -1, size = e$fonte / 3.4, family = e$familia) + folga +
      tt("Esforço e riqueza", "Esforço (horas)", "Riqueza (espécies)")
    texto <- list(mede = "Correlação de Spearman entre as horas de busca e o número de espécies de cada unidade.",
      resultado = sprintf("rho = %s; p %s (%d unidades).", fmt(ct$estimate, 2), fp(ct$p.value), nrow(d)),
      interpretacao = if (ct$p.value >= .05) "Não houve evidência de associação monotônica entre esforço e riqueza." else if (ct$estimate > 0) "A riqueza observada aumenta com o esforço nas unidades avaliadas." else "A riqueza observada diminui com o esforço nas unidades avaliadas.",
      limites = "Com poucas unidades, só correlações fortes aparecem. A reta é só visual (lm).")
  }
}

if (analise == "mantel") {
  pc <- if (!is.null(P)) { q <- P; q$ponto <- ponto_fisico(q$ponto); q[!duplicated(q$ponto), ] } else NULL
  D <- tryCatch(distancias_pontos(pc), error = function(x) NULL)
  mp <- m; if (multi) { f <- ponto_fisico(un); mp <- rowsum(m, f) }
  comum <- if (is.null(D)) character() else intersect(rownames(mp), rownames(D))
  if (length(comum) < 5) pr("Coordenadas de 5 ou mais pontos", "falha", sprintf("Só %d pontos com coordenadas legíveis na aba de pontos.", length(comum)))
  else { pr("Coordenadas de 5 ou mais pontos", "ok", sprintf("%d pontos.", length(comum)))
    np <- factorial(min(length(comum), 7)); pr("Permutações", if (np < 1000) "atenção" else "ok", sprintf("Até %d rearranjos, além da ordem observada.", min(np - 1, 999))) }
  if (!falhou()) {
    dgeo <- as.numeric(stats::as.dist(D[comum, comum]))
    dbio <- as.numeric(vegan::vegdist(mp[comum, , drop = FALSE] > 0, "jaccard"))
    if (any(!is.finite(dgeo))) pr("Coordenadas comparáveis", "falha", "Há pares sem distância geográfica válida. Confira o sistema, zona e hemisfério das coordenadas.")
    else if (length(unique(dgeo)) < 2 || length(unique(dbio)) < 2) pr("Variação nas distâncias", "falha", "As distâncias geográficas e biológicas precisam variar.")
  }
  if (multi) pr("Campanhas somadas", "atenção", "Com várias campanhas, cada ponto soma todas as campanhas (a distância é do ponto, não da campanha).")
  if (!falhou()) {
    dj <- vegan::vegdist(mp[comum, ] > 0, "jaccard"); dg <- stats::as.dist(D[comum, comum])
    mt <- vegan::mantel(dg, dj, method = "spearman", permutations = 999)
    tabela <- data.frame(r_mantel = round(mt$statistic, 3), p = signif(mt$signif, 3), pontos = length(comum))
    dd <- data.frame(distancia_km = as.vector(dg) / 1000, jaccard = as.vector(dj))
    fig <- ggplot(dd, aes(distancia_km, jaccard)) + geom_point(size = e$ponto * 1.2, colour = pal[1]) + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = pal[2], linewidth = e$linha) +
      tt("Distância e diferença de composição", "Distância entre pontos (km)", "Dissimilaridade de Jaccard")
    texto <- list(mede = "Testa se pontos mais distantes têm composições mais diferentes (Jaccard, presença).",
      resultado = sprintf("r de Mantel = %s; p %s (%d pontos).", fmt(mt$statistic, 2), fp(mt$signif), length(comum)),
      interpretacao = if (mt$signif < .05) "A distância explica parte da diferença: pontos próximos não são independentes; leve isso em conta ao comparar ambientes." else "Sem relação clara entre distância e composição.",
      limites = "Com 5 a 7 pontos o teste tem pouco poder. Distância em linha reta, sem barreiras.")
  }
}

if (analise == "pca") {
  if (n < 3 || ncol(m) < 3) pr("3 ou mais unidades e espécies", "falha", sprintf("%d unidades e %d espécies.", n, ncol(m))) else pr("3 ou mais unidades e espécies", "ok", sprintf("%d unidades e %d espécies.", n, ncol(m)))
  if (n > 0 && !any(apply(vegan::decostand(m, "hellinger"), 2, function(x) diff(range(x)) > sqrt(.Machine$double.eps))))
    pr("Variação de composição", "falha", "As proporções das espécies são constantes entre todas as unidades.")
  pr("Transformação de Hellinger", "ok", "Aplicada (Legendre e Gallagher 2001): reduz o peso dos zeros e das espécies muito abundantes.")
  if (!falhou()) {
    pc <- stats::prcomp(vegan::decostand(m, "hellinger")); ve <- pc$sdev^2 / sum(pc$sdev^2)
    sc <- as.data.frame(pc$x[, 1:2]); sc$unidade <- rownames(sc); sc$grupo <- if (!is.null(g)) g[rownames(sc)] else "Unidades"
    ld <- as.data.frame(pc$rotation[, 1:2]); ld$especie <- rownames(ld); ld <- head(ld[order(-(ld$PC1^2 + ld$PC2^2)), ], 8)
    f <- max(abs(sc$PC1), abs(sc$PC2)) / max(abs(ld$PC1), abs(ld$PC2)) * .8
    # afasta na vertical os nomes do mesmo lado que ficariam encavalados
    ld$ly <- ld$PC2 * f; gap <- diff(range(c(sc$PC2, ld$ly))) * .055
    for (lado in c(TRUE, FALSE)) { ii <- which((ld$PC1 > 0) == lado); ii <- ii[order(ld$ly[ii])]
      if (length(ii) > 1) for (j in 2:length(ii)) if (ld$ly[ii[j]] - ld$ly[ii[j - 1]] < gap) ld$ly[ii[j]] <- ld$ly[ii[j - 1]] + gap }
    tabela <- data.frame(eixo = paste0("PC", seq_along(ve)), variancia_explicada = round(ve, 3), acumulada = round(cumsum(ve), 3))[seq_len(min(5, length(ve))), ]
    fig <- ggplot(sc, aes(PC1, PC2)) + geom_hline(yintercept = 0, colour = k$grade) + geom_vline(xintercept = 0, colour = k$grade) +
      geom_segment(data = ld, aes(x = 0, y = 0, xend = PC1 * f, yend = PC2 * f), colour = k$tinta2, arrow = grid::arrow(length = grid::unit(.15, "cm")), linewidth = .4) +
      geom_text(data = ld, inherit.aes = FALSE, aes(PC1 * f, ly, label = especie, hjust = ifelse(PC1 > 0, -.05, 1.05)), size = e$fonte / 3.8, fontface = "italic", family = e$familia, colour = k$tinta2) +
      geom_point(aes(colour = grupo), size = e$ponto * 1.3) + geom_label(aes(label = unidade, colour = grupo), vjust = 1.6, size = e$fonte / 3.4, family = e$familia, show.legend = FALSE, fill = k$fundo, linewidth = 0, label.padding = grid::unit(1, "pt")) +
      scale_colour_manual(values = cor_g(sc$grupo), name = res$fator_nome) + folga +
      tt("PCA (Hellinger)", sprintf("PC1 (%s)", paste0(fmt(100 * ve[1], 0), "%")), sprintf("PC2 (%s)", paste0(fmt(100 * ve[2], 0), "%")))
    if (is.null(g)) fig <- fig + theme(legend.position = "none")
    texto <- list(mede = "Resume a variação da comunidade em eixos; as setas mostram as espécies que mais pesam.",
      resultado = sprintf("Os dois primeiros eixos explicam %s da variação.", paste0(fmt(100 * sum(ve[1:2]), 0), "%")),
      interpretacao = "Unidades do mesmo lado de uma seta têm mais daquela espécie.", limites = "Descritiva; não testa diferenças. Só as 8 espécies de maior peso aparecem.")
  }
}

if (analise == "quiquadrado") {
  if (repetido) falha_repeticao()
  v <- rowSums(m_total)
  ef <- if (is.null(P$esforco)) rep(NA_real_, length(v)) else suppressWarnings(as.numeric(P$esforco[match(names(v), P$ponto)]))
  com_esforco <- is.finite(ef) & ef > 0
  incluir <- v > 0 | com_esforco
  v <- v[incluir]; ef <- ef[incluir]; com_esforco <- com_esforco[incluir]
  if (length(v) < 2 || sum(v) <= 0) pr("Duas unidades ou mais", "falha", "São necessárias duas unidades amostradas e pelo menos um indivíduo.")
  if (any(com_esforco) && !all(com_esforco))
    pr("Esforço comparável", "falha", "Só parte das unidades tem esforço positivo informado. Complete os esforços para calcular as frequências esperadas.")
  if (length(v) && all(com_esforco)) {
    prob <- ef / sum(ef)
    criterio <- "A hipótese é uma taxa de registros constante por hora entre pontos, com contagens esperadas proporcionais ao esforço."
  } else {
    prob <- rep(1 / length(v), length(v))
    criterio <- "A hipótese é de contagens iguais entre as unidades com registros. Sem esforço documentado, este contraste não testa igualdade de densidade."
    pr("Esforço de amostragem", "atenção", criterio)
  }
  esperado <- sum(v) * prob
  pr("Contagens", "ok", sprintf("%d indivíduos em %d unidades.", sum(v), length(v)))
  pr("Indivíduos independentes", "atenção", "Indivíduos de coros ou bandos podem ser dependentes. A inferência depende de como foram contados.")
  if (!falhou()) {
    baixa <- any(esperado < 5)
    pr("Frequências esperadas", if (baixa) "atenção" else "ok",
       if (baixa) "Há frequências esperadas abaixo de cinco. O p é estimado por 2.000 simulações multinomiais." else "Todas as frequências esperadas são de pelo menos cinco.")
    ch <- stats::chisq.test(v, p = prob, simulate.p.value = baixa, B = 2000)
    tabela <- data.frame(unidade = names(v), observado = as.numeric(v), esperado = round(esperado, 2),
                         residuo_padronizado = round(as.numeric(ch$stdres), 2))
    fig <- ggplot(tabela, aes(unidade, observado)) + geom_col(fill = pal[1], width = .65) +
      geom_point(aes(y = esperado), colour = pal[2], shape = 18, size = e$ponto * 1.4) +
      geom_text(aes(label = observado), vjust = -.4, size = e$fonte / 3.2, family = e$familia) +
      scale_y_continuous(expand = expansion(mult = c(0, .12))) +
      tt("Contagens observadas (barras) e esperadas (losangos)", NULL, "Indivíduos")
    texto <- list(mede = criterio,
      resultado = sprintf("Qui-quadrado = %s; p %s.", fmt(ch$statistic, 2), fp(ch$p.value)),
      interpretacao = if (ch$p.value < .05) "As contagens observadas diferem da distribuição especificada na hipótese." else "Sem evidência de diferença em relação à distribuição esperada.",
      limites = "O resultado depende de contagens independentes e da comparabilidade do método e da detecção.")
  }
}

# ---- gravar
cat("@@ 85 Gravando o resultado\n")
dir.create(file.path(saida, "pedidos"), showWarnings = FALSE)
pid <- paste(c(gsub("[^A-Za-z]", "", iconv(grupo, "UTF-8", "ASCII//TRANSLIT")), analise, if (nzchar(fator)) fator, if (analise %in% c("kruskal", "anova", "regressao_gradiente")) variavel), collapse = "_")
res$pid <- pid; res$pressupostos <- PR; res$rodou <- !falhou() && (!is.null(tabela) || !is.null(fig)); res$texto <- texto
res$tabela <- if (is.data.frame(tabela)) list(principal = tabela) else tabela
res$figura <- NULL
if (!is.null(fig) && res$rodou) {
  arq <- file.path(saida, "pedidos", paste0(pid, ".png")); tp <- if (.Platform$OS.type == "windows") "windows" else "cairo"
  w <- e$largura; h <- e$altura
  ggsave(arq, fig, width = w, height = h, dpi = e$dpi, bg = k$fundo, device = "png", type = tp)
  ggsave(sub("\\.png$", ".jpg", arq), fig, width = w, height = h, dpi = e$dpi_jpeg %||% 600, bg = k$fundo, device = "jpeg", type = tp, quality = 100)
  res$figura <- paste0("pedidos/", pid, ".png")
}
write_json(res, file.path(saida, "pedidos", paste0(pid, ".json")), auto_unbox = TRUE, na = "null", digits = NA, dataframe = "rows", pretty = TRUE)
fi <- file.path(saida, "pedidos", "indice.json")
ind <- if (file.exists(fi)) tryCatch(fromJSON(fi, simplifyVector = FALSE), error = function(x) list()) else list()
ind <- Filter(function(z) !identical(z$pid, pid), ind)
ind <- c(list(list(pid = pid, id = analise, nome = info$nome, grupo = grupo, fator = res$fator_nome, rodou = res$rodou, feito_em = res$feito_em)), ind)
write_json(ind, fi, auto_unbox = TRUE, pretty = TRUE)
cat(if (res$rodou) "Pronto: análise rodada." else "Não rodou: um pressuposto não foi atendido (veja no painel).", "\n")
cat("@@ 100 Pronto\n")
