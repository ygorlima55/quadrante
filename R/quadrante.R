# Quadrante: análise de comunidades de fauna no R a partir da planilha de campo.
# Uso: Rscript quadrante.R "<planilha.xlsx>" ["<pasta de saída>"]
# Saída: painel.html (abre no navegador), figuras/ (PNG 300 dpi para o laudo), tabelas_quadrante.xlsx, dados.js.

# pacotes do R: pasta r-pacotes do programa (criada pelo instalar.cmd), ou HERP_RLIB, ou a biblioteca da máquina
local({ raiz <- dirname(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))))
  libs <- c(Sys.getenv("HERP_RLIB"), file.path(raiz, "r-pacotes"), "C:/DadosBio/envs/R-herpetodados")
  .libPaths(c(libs[nzchar(libs) & dir.exists(libs)], .libPaths())) })
suppressPackageStartupMessages({ library(vegan); library(iNEXT); library(ggplot2); library(readxl); library(writexl); library(jsonlite) })
set.seed(20261117)

args <- commandArgs(TRUE)
aqui <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
for (f in c("ler.R", "conferir.R", "desenho.R", "analises.R", "descritiva.R", "saida.R", "ajustes_fig.R", "saida_extra.R", "saida_novas.R", "campanhas.R", "fitofisionomias.R", "correcoes.R", "relatorio_fauna.R", "contexto.R", "secundarios.R")) source(file.path(aqui, f), encoding = "UTF-8")
if (!length(args)) stop("Informe a planilha: Rscript quadrante.R planilha.xlsx")
planilha <- normalizePath(args[1], mustWork = TRUE)
base <- tools::file_path_sans_ext(basename(planilha))
saida <- if (length(args) >= 2) args[2] else file.path(dirname(planilha), paste0("Quadrante_", base))
dir.create(file.path(saida, "figuras"), recursive = TRUE, showWarnings = FALSE)
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

progresso(2, "Lendo a planilha")
msg("Lendo a planilha:", basename(planilha))
L <- ler_planilha(planilha)
msg(nrow(L$registros), "registros na aba", L$aba_registros)

# correções feitas no painel (botão Corrigir), guardadas em correcoes.json
correcoes <- ler_correcoes(file.path(saida, "correcoes.json"))
if (length(correcoes)) { L <- aplicar_correcoes(L, correcoes); msg(length(correcoes), "correções do painel aplicadas") }
progresso(6, "Conferindo a planilha (corretor)")
msg("Conferindo os dados (corretor)...")
conf <- conferir_planilha(L)
# o que só está na consolidada entra nas análises (e fica listado na conferência)
L <- completar_com_consolidada(L)
if (nrow(L$completados)) msg(nrow(L$completados), "registros completados pela aba consolidada:", paste(sprintf("%s %s (+%s)", L$completados$especie, L$completados$ponto, L$completados$adicionados), collapse = "; "))
conf <- rbind(conferencia_completados(L), conf)
if (!is.null(L$secundarios) && nrow(L$secundarios)) conf <- rbind(conf, data.frame(nivel = "informação", aba = L$aba_registros, linha = paste(L$secundarios$linha_planilha, collapse = ", "),
  campo = "Dado secundário", problema = sprintf("%d registros marcados como dado secundário (bibliografia, entrevista) ficaram fora das análises.", nrow(L$secundarios)),
  sugestao = "Eles aparecem na planilha 09, aba Dados secundários.", tipo = "", especie = "", ponto = "", dados = "", stringsAsFactors = FALSE))
unlink(file.path(saida, "planilha_corrigida.xlsx"))
tryCatch(gravar_planilha_corrigida(L, saida), error = function(e) msg("planilha corrigida não gravada:", conditionMessage(e)))
# registros com erro que quebrariam ou distorceriam as contas (espécie vazia, Total inválido) saem das análises e ficam listados
L <- registros_para_analise(L)
if (nrow(L$fora_da_analise)) {
  conf <- rbind(conf, data.frame(nivel = "informação", aba = L$aba_registros, linha = paste(L$fora_da_analise$linha_planilha, collapse = ", "),
    campo = "Fora das análises", problema = sprintf("%d registros ficaram fora das análises até serem corrigidos: %s.", nrow(L$fora_da_analise),
      paste(sprintf("linha %s (%s)", L$fora_da_analise$linha_planilha, L$fora_da_analise$motivo), collapse = "; ")),
    sugestao = "Corrigir na planilha e usar Atualizar planilha. Os erros de cada linha estão acima nesta conferência.", tipo = "", especie = "", ponto = "", dados = "", stringsAsFactors = FALSE))
  msg(nrow(L$fora_da_analise), "registros com erro ficaram fora das análises")
}
if (!nrow(L$registros)) stop("Nenhum registro válido para analisar: confira as colunas Espécie e Total da aba de registros.")
msg(sum(conf$nivel == "erro"), "erros,", sum(conf$nivel == "atenção"), "pontos de atenção,", sum(conf$nivel == "informação"), "informações")

campanhas_cadastradas <- na.omit(L$pontos$campanha)
if (length(unique(c(L$registros$campanha, campanhas_cadastradas[nzchar(campanhas_cadastradas)]))) >= 2) {
  L <- unidades_campanha(L)
  msg(nrow(L$campanhas), "campanhas:", paste(L$campanhas$codigo, L$campanhas$campanha, sep = " = ", collapse = "; "), "| unidade amostral = ponto x campanha")
}
# contexto do campo escrito no painel: estação e esforço quando a planilha não traz (a planilha sempre vence)
ctx <- ler_contexto(saida)
if (!is.null(ctx)) { L <- aplicar_contexto(L, ctx); if (isTRUE(L$esforco_do_contexto)) msg("Esforço por ponto tirado do contexto do campo"); if (isTRUE(L$estacao_do_contexto)) msg("Estação dos registros tirada do contexto do campo") }
# um conjunto de análises por grupo de fauna (Anuros, Répteis, Aves, Mamíferos, Insetos... ou o que vier na coluna Grupo)
gs <- unique(L$registros$grupo_pad); ordem <- c("Anuros", "Répteis", "Aves", "Mamíferos", "Peixes", "Insetos", "Aracnídeos")
gs <- gs[order(match(gs, ordem, nomatch = 99), gs)]
grupos <- setNames(as.list(gs), gs)
if (length(gs) > 1) grupos[[if (all(gs %in% c("Anuros", "Répteis"))) "Herpetofauna" else "Todos os grupos"]] <- gs
R <- list()
for (g in names(grupos)) {
  reg <- L$registros[L$registros$grupo_pad %in% grupos[[g]], ]
  if (!nrow(reg)) next
  progresso(10 + 50 * (match(g, names(grupos)) - 1) / length(grupos), paste0("Análises de ", g, " (acumulação, estimadores, iNEXT, similaridade)"))
  msg("Analisando:", g)
  i <- match(g, names(grupos)); R[[g]] <- analisar_grupo(reg, g, L$pontos, consol_atributos(L), L$campanhas, faixa = 10 + 50 * c(i - 1, i) / length(grupos))
}
avisos <- avisos_pressupostos(L, R)
dist <- distancias_pontos(L$pontos_campo %||% L$pontos)
if (!is.null(dist) && any(is.finite(dist[upper.tri(dist)]))) {
  d <- dist; diag(d) <- NA; k <- which(d == min(d, na.rm = TRUE), arr.ind = TRUE)[1, ]
  avisos[[length(avisos) + 1]] <- list(tema = "Distância entre pontos", nivel = if (min(d, na.rm = TRUE) < 500) "atenção" else "informação",
    texto = sprintf("A menor distância entre pontos é %s m (%s e %s); a maior é %s m. %s", format(min(d, na.rm = TRUE), big.mark = ".", decimal.mark = ","), rownames(d)[k[1]], colnames(d)[k[2]],
                    format(max(d, na.rm = TRUE), big.mark = ".", decimal.mark = ","), if (min(d, na.rm = TRUE) < 500) "Cantos (anuros, aves) são ouvidos a várias centenas de metros e animais que se deslocam podem passar por mais de um ponto: pontos tão próximos podem registrar os mesmos indivíduos." else "A independência também depende da espécie, do método e do período de amostragem."))
}

if (!is.null(dist)) for (g in names(R)) if (!is.null(R[[g]]$similaridade$pares)) {
  pp <- R[[g]]$similaridade$pares; pa <- sub(" C[0-9]+$", "", pp$ponto_a); pb <- sub(" C[0-9]+$", "", pp$ponto_b)
  ok <- pa %in% rownames(dist) & pb %in% rownames(dist)
  pp$distancia_m[ok] <- dist[cbind(pa[ok], pb[ok])]; R[[g]]$similaridade$pares <- pp
}
estilo <- ler_estilo(c(file.path(saida, "estilo.json"), file.path(dirname(aqui), "estilo.json")))
msg("Gerando figuras (ggplot2)...")
figs <- gerar_figuras(R, lapply(R, attr, "objs"), saida, estilo)
tit_padrao <- titulos_padrao()  # antes das miniaturas, que desenham as outras formas
formas <- tryCatch(gerar_formas(R, lapply(R, attr, "objs"), saida, estilo), error = function(x) { msg("miniaturas falharam:", conditionMessage(x)); list() })
saveRDS(list(R = R, objs = lapply(R, attr, "objs")), file.path(saida, "resultados.rds"))
for (g in names(R)) attr(R[[g]], "objs") <- NULL

progresso(96, "Gravando as tabelas em Excel")
msg("Gravando tabelas em Excel...")
tabs <- exportar_tabelas(L, conf, R, saida); unlink(file.path(saida, "tabelas_quadrante.xlsx"))
tryCatch(exportar_relatorio_fauna(L, R, saida), error = function(e) msg("planilha 09 não gravada:", conditionMessage(e)))

# dados secundários (literatura da região): secundarios/regiao.json (painel) e secundarios/dados_secundarios.xlsx (agente ou à mão)
SEC <- tryCatch({
  arq_sec <- file.path(saida, "secundarios", "dados_secundarios.xlsx"); arq_reg <- file.path(saida, "secundarios", "regiao.json")
  regiao <- if (file.exists(arq_reg)) jsonlite::fromJSON(arq_reg) else NULL
  if (!file.exists(arq_sec)) modelo_secundarios(arq_sec)
  S <- ler_secundarios(arq_sec)
  A_sec <- if (nrow(S$obras)) analisar_secundarios(S, L$registros, regiao) else NULL
  F_sec <- if (!is.null(A_sec)) { progresso(97, "Dados secundários: figuras e tabelas"); exportar_secundarios(A_sec, saida); figuras_secundarios(A_sec, saida, estilo) } else list()
  if (!is.null(A_sec)) msg("Dados secundários:", A_sec$resumo$obras_usadas, "obras,", A_sec$resumo$riqueza_secundaria, "espécies")
  list(regiao = regiao, centro_sugerido = centro_pontos(L$pontos), analise = A_sec, figuras = F_sec, avisos = rbind(S$avisos, if (!is.null(A_sec)) A_sec$avisos))
}, error = function(e) { msg("dados secundários não analisados:", conditionMessage(e)); list(centro_sugerido = tryCatch(centro_pontos(L$pontos), error = function(x) NULL), erro = conditionMessage(e)) })
tabs <- list.files(file.path(saida, "tabelas"))

ctx_arq <- file.path(saida, "contexto_campo.txt")
contexto <- if (file.exists(ctx_arq)) paste(readLines(ctx_arq, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
interp_arq <- file.path(saida, "leitura_especialista.md")
leitura <- if (file.exists(interp_arq)) paste(readLines(interp_arq, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""

ctx_txt <- tryCatch(texto_contexto(ctx, L), error = function(e) { msg("texto do contexto não gerado:", conditionMessage(e)); NULL })
tryCatch(gravar_contexto_md(saida, ctx, ctx_txt, R, L), error = function(e) msg("contexto_campo.md não gravado:", conditionMessage(e)))
pacotes <- sapply(c("vegan", "iNEXT", "ggplot2", "readxl"), function(p) as.character(packageVersion(p)))
out <- list(
  meta = list(planilha = if (nzchar(Sys.getenv("HERP_NOME"))) Sys.getenv("HERP_NOME") else basename(planilha), caminho = planilha, gerado_em = format(Sys.time(), "%d/%m/%Y %H:%M"),
              r = R.version.string, pacotes = as.list(pacotes), abas = list(registros = L$aba_registros, pontos = L$aba_pontos, consolidada = L$aba_consolidada)),
  campanhas = L$campanhas, completados = L$completados, correcoes = correcoes, correcoes_aplicadas = L$correcoes_aplicadas,
  tem_planilha_corrigida = file.exists(file.path(saida, "planilha_corrigida.xlsx")), pontos = L$pontos, gradientes = catalogo_gradientes(L$pontos), distancias = dist, conferencia = conf, avisos = avisos, grupos = R, figuras = figs, formas = formas, catalogo = catalogo_todo(), tabelas = tabs,
  estilo = estilo, titulos_padrao = tit_padrao, secundarios = SEC, contexto = contexto, contexto_resumo = ctx_txt, leitura = leitura,
  registros = L$registros[, c("linha_planilha", "ponto", "especie", "especie_completa", "familia", "grupo_pad", "vo", "av", "sem_forma", "total", "grupo", "ponto_original", intersect("alterado", names(L$registros)), if (!is.null(L$campanhas)) c("campanha", "ponto_campo"))]
)
js <- jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", digits = NA, dataframe = "rows", pretty = FALSE)
writeLines(c("window.HERP = ", js, ";"), file.path(saida, "dados.js"), useBytes = TRUE)
file.copy(file.path(dirname(aqui), "painel", "painel.html"), file.path(saida, "painel.html"), overwrite = TRUE)
A <- R[[tail(names(grupos), 1L)]]
resumo <- list(planilha = out$meta$planilha, gerado_em = out$meta$gerado_em, grupos = names(R),
  riqueza = A$riqueza, individuos = A$n_individuos, pontos = A$n_pontos,
  chao2 = if (!is.null(A$acumulacao)) A$acumulacao$estimadores$valor[2] else NA,
  erros = sum(conf$nivel == "erro"), atencao = sum(conf$nivel == "atenção"),
  campanhas = if (is.null(L$campanhas)) 1L else nrow(L$campanhas))
writeLines(jsonlite::toJSON(resumo, auto_unbox = TRUE, na = "null"), file.path(saida, "resumo.json"), useBytes = TRUE)
progresso(100, "Pronto")
msg("Pronto. Abra:", file.path(saida, "painel.html"))
