# Quadrante: contexto do campo (19/11/2026, pedido do usuário). O formulário "Contexto do campo" do painel grava
# contexto.json na pasta do levantamento:
#   servico (diagnostico, monitoramento, inventario, resgate, pesquisa, outro), finalidade, campo (observações livres),
#   campanhas: nome, inicio, fim, estacao (chuvosa, seca, transicao), observadores, dias, horas_ponto (horas de busca
#   em cada ponto por visita), visitas (visitas a cada ponto na campanha), horario, metodos, clima.
# O R usa o contexto para: (1) estação de cada registro, quando a planilha não tem a coluna Estação;
# (2) esforço por ponto (observadores x horas por ponto x visitas), quando a aba de pontos não tem Esforço;
# (3) o texto de métodos e esforço do laudo; (4) contexto_campo.md, que os agentes (corretor, leitura) leem.
# A planilha sempre vence: o contexto só preenche o que ela não traz.

num_ctx <- function(x) { v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x %||% NA)))); if (!length(v)) NA_real_ else v }
data_br <- function(x) { x <- as.character(x %||% ""); if (!nzchar(x) || is.na(x)) return(""); d <- tryCatch(as.Date(x), error = function(e) NA)
  if (is.na(d)) x else format(d, "%d/%m/%Y") }
ESTACOES <- c(chuvosa = "Chuvosa", seca = "Seca", transicao = "Transição")
SERVICOS <- c(diagnostico = "diagnóstico ecológico", monitoramento = "monitoramento de fauna",
              inventario = "inventário ou levantamento rápido", resgate = "resgate e afugentamento de fauna",
              pesquisa = "pesquisa científica", outro = "outro estudo ecológico")

ler_contexto <- function(saida) {
  f <- file.path(saida, "contexto.json")
  if (!file.exists(f)) return(NULL)
  ctx <- tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(ctx)) return(NULL)
  cs <- ctx$campanhas
  ctx$campanhas <- if (is.data.frame(cs) && nrow(cs)) cs else NULL
  ctx
}

# índice da campanha do contexto para cada valor de campanha da planilha (ou a única campanha do contexto)
campanha_ctx <- function(cs, valores) {
  if (is.null(cs)) return(rep(NA_integer_, length(valores)))
  nm <- sem_acento(trimws(as.character(cs$nome %||% rep("", nrow(cs))))); cd <- sem_acento(trimws(as.character(cs$codigo %||% rep("", nrow(cs)))))
  v <- sem_acento(trimws(as.character(valores)))
  i <- match(v, nm); j <- match(v, cd); k <- ifelse(is.na(i), j, i)
  if (nrow(cs) == 1 && all(is.na(v) | !nzchar(v) | v == "campanha unica")) k[] <- 1L
  k
}

aplicar_contexto <- function(L, ctx) {
  cs <- ctx$campanhas
  if (is.null(cs)) return(L)
  r <- L$registros
  # 1. estação
  falta <- is.na(r$estacao) | !nzchar(r$estacao)
  if (any(falta) && !is.null(cs$estacao)) {
    k <- campanha_ctx(cs, r$campanha); est <- unname(ESTACOES[as.character(cs$estacao[k])])
    preencher <- falta & !is.na(est)
    if (any(preencher)) { r$estacao[preencher] <- est[preencher]; L$registros <- r; L$estacao_do_contexto <- TRUE }
  }
  # 2. esforço por ponto (horas-pessoa)
  if (!is.null(L$pontos) && any(is.na(L$pontos$esforco))) {
    vis <- num_ctx(cs$visitas); vis[is.na(vis)] <- 1
    h <- num_ctx(cs$observadores) * num_ctx(cs$horas_ponto) * vis
    if (any(!is.na(h))) {
      if (!is.null(L$campanhas) && is.null(L$pontos$campanha)) {
        # ponto com as campanhas somadas: esforço = soma das campanhas em que o ponto foi visitado
        vis_p <- unique(rbind(data.frame(ponto = r$ponto, campanha = r$campanha), if (!is.null(L$visitas_campanha)) L$visitas_campanha[, c("ponto", "campanha")]))
        hp <- vapply(L$pontos$ponto, function(p) { cc <- unique(vis_p$campanha[vis_p$ponto == p]); if (!length(cc)) cc <- unique(r$campanha)
          kk <- campanha_ctx(cs, cc); v <- h[kk]; if (all(!is.finite(v))) NA_real_ else sum(v, na.rm = TRUE) }, 1)
      } else {
        campanha <- if (!is.null(L$pontos$campanha)) L$pontos$campanha else rep(r$campanha[1], nrow(L$pontos))
        hp <- h[campanha_ctx(cs, campanha)]
      }
      preencher <- is.na(L$pontos$esforco) & is.finite(hp) & hp > 0
      if (any(preencher)) { L$pontos$esforco[preencher] <- hp[preencher]; L$esforco_do_contexto <- TRUE }
    }
  }
  L
}

# texto de métodos e esforço, com as campanhas descritas uma a uma
texto_contexto <- function(ctx, L) {
  if (is.null(ctx)) return(NULL)
  cs <- ctx$campanhas; partes <- character()
  sv <- SERVICOS[ctx$servico %||% ""]; if (!is.na(sv) && length(sv)) partes <- c(partes, sprintf("Tipo de estudo: %s.", sv))
  if (nzchar(ctx$finalidade %||% "")) partes <- c(partes, sprintf("Finalidade: %s", sub("([^.])$", "\\1.", trimws(ctx$finalidade))))
  np <- if (!is.null(L$pontos_campo)) nrow(L$pontos_campo) else if (!is.null(L$pontos)) nrow(L$pontos) else length(unique(L$registros$ponto))
  tab <- NULL
  if (!is.null(cs)) {
    tab <- do.call(rbind, lapply(seq_len(nrow(cs)), function(i) {
      c1 <- cs[i, , drop = FALSE]; ob <- num_ctx(c1$observadores); hp <- num_ctx(c1$horas_ponto); vi <- num_ctx(c1$visitas); if (is.na(vi)) vi <- 1
      data.frame(campanha = c1$nome %||% paste("Campanha", i), inicio = data_br(c1$inicio), fim = data_br(c1$fim),
        estacao = unname(ESTACOES[as.character(c1$estacao %||% "")]) %||% "", observadores = ob, dias = num_ctx(c1$dias),
        horas_por_ponto = hp, visitas_por_ponto = vi, pontos = np, esforco_horas_pessoa = ob * hp * vi * np,
        metodos = paste(unlist(c1$metodos), collapse = ", "), horario = c1$horario %||% "", clima = c1$clima %||% "", stringsAsFactors = FALSE)
    }))
    tab$estacao[is.na(tab$estacao)] <- ""
    for (i in seq_len(nrow(tab))) with(tab[i, ], {
      pd <- if (nzchar(inicio) && nzchar(fim) && inicio != fim) sprintf("de %s a %s", inicio, fim) else if (nzchar(inicio)) sprintf("em %s", inicio) else ""
      bits <- c(pd, if (nzchar(estacao)) paste("estação", tolower(estacao)), if (!is.na(observadores)) sprintf("%s observador%s", fmt(observadores, 0), if (observadores > 1) "es" else ""),
                if (!is.na(dias)) sprintf("%s dia%s de campo", fmt(dias, 0), if (dias > 1) "s" else ""), if (nzchar(horario)) paste("horário", horario),
                if (nzchar(metodos)) paste("métodos:", metodos))
      ef <- if (!is.na(esforco_horas_pessoa)) sprintf(" Esforço: %s h de busca por ponto e visita, %s visita%s, %s horas-pessoa nos %d pontos.", fmt(horas_por_ponto, 1), fmt(visitas_por_ponto, 0), if (visitas_por_ponto > 1) "s" else "", fmt(esforco_horas_pessoa, 1), pontos) else ""
      partes <<- c(partes, sprintf("%s: %s.%s%s", campanha, paste(bits[nzchar(bits)], collapse = ", "), ef, if (nzchar(clima)) paste0(" Tempo: ", sub("([^.])$", "\\1.", clima)) else ""))
    })
    tot <- sum(tab$esforco_horas_pessoa, na.rm = TRUE)
    if (nrow(tab) > 1 && tot > 0) partes <- c(partes, sprintf("Esforço total: %s horas-pessoa em %d campanhas.", fmt(tot, 1), nrow(tab)))
    est <- unique(as.character(L$registros$estacao))
    est <- est[!is.na(est) & nzchar(est)]
    if (length(est) == 1) partes <- c(partes, sprintf("Os registros com estação informada descrevem o período %s.", tolower(est)))
    if (length(est) > 1) partes <- c(partes, sprintf("Estações informadas nos registros: %s.", paste(tolower(est), collapse = ", ")))
  }
  nplan <- length(unique(L$registros$campanha[!is.na(L$registros$campanha) & nzchar(L$registros$campanha)]))
  if (!is.null(cs) && nrow(cs) > max(1, nplan)) partes <- c(partes, sprintf("O contexto descreve %d campanhas, mas a planilha tem registros de %s: as comparações usam as visitas documentadas na planilha.", nrow(cs), if (nplan <= 1) "uma só" else nplan))
  if (isTRUE(L$esforco_do_contexto)) partes <- c(partes, "O esforço por ponto veio do contexto do campo (observadores x horas por ponto x visitas), porque a aba de pontos não tem a coluna Esforço.")
  if (isTRUE(L$estacao_do_contexto)) partes <- c(partes, "A estação de cada registro veio do contexto do campo, porque a planilha não tem a coluna Estação.")
  if (nzchar(ctx$campo %||% "")) partes <- c(partes, paste("Observações de campo:", trimws(ctx$campo)))
  list(texto = paste(partes, collapse = " "), campanhas = tab, servico = ctx$servico %||% "")
}

# arquivo para os agentes (corretor da planilha, leitura do especialista, assistente): contexto + números principais
gravar_contexto_md <- function(saida, ctx, txt, R, L) {
  ln <- c("# Contexto do campo", "", "Gerado pelo Quadrante. Agentes: leiam este arquivo antes de interpretar os resultados deste levantamento.", "")
  if (!is.null(txt)) ln <- c(ln, "## Como foi o campo", "", txt$texto, "")
  if (!is.null(txt$campanhas)) {
    t <- txt$campanhas
    ln <- c(ln, "| Campanha | Início | Fim | Estação | Observadores | Dias | h/ponto | Visitas | Horas-pessoa | Métodos |", "|---|---|---|---|---|---|---|---|---|---|",
      sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |", t$campanha, t$inicio, t$fim, t$estacao, t$observadores, t$dias, t$horas_por_ponto, t$visitas_por_ponto, t$esforco_horas_pessoa, t$metodos), "")
  }
  ln <- c(ln, "## Números principais", "")
  for (g in names(R)) { x <- R[[g]]
    ln <- c(ln, sprintf("- %s: %s espécies, %s indivíduos, %s pontos%s.", g, x$riqueza %||% "?", x$n_individuos %||% "?", x$n_pontos %||% "?",
      if (!is.null(x$acumulacao$estimadores)) sprintf("; Chao 2 = %s", fmt(x$acumulacao$estimadores$valor[2], 1)) else "")) }
  ln <- c(ln, "", "Arquivos: dados.js (todos os resultados), tabelas/ (Excel), figuras/ (PNG e JPEG), conferencia na planilha 00.")
  writeLines(enc2utf8(ln), file.path(saida, "contexto_campo.md"), useBytes = TRUE)
}
