# Quadrante

Análises de comunidade de fauna **feitas no R** a partir da planilha de campo, sem precisar abrir o RStudio nem escrever código.
O catálogo atende a inventários, monitoramentos, pesquisas e outros estudos de comunidades de fauna. A escolha da análise depende da pergunta ecológica e dos dados disponíveis.

Você solta a planilha numa página do navegador, o R confere os dados, roda as análises e monta um painel com as figuras, as tabelas e a explicação de cada resultado em português.

## O que ele faz
| Tema | Como |
|---|---|
| Conferência da planilha | aponta erros e pontos de atenção (aba, linha e o que fazer); botão **Corrigir** pergunta o que falta e corrige; compara a aba consolidada com os registros e completa o que faltar; quadro para preencher atributos que faltam; gera a planilha corrigida com o histórico |
| Descritiva | famílias, espécies, riqueza e abundância por família e por ponto, forma de registro, atributos das espécies |
| Riqueza | curva de acumulação (`vegan::specaccum`), Chao 2, Jackknife 1 e 2, Bootstrap (`poolaccum`), no estilo da curva do iNEXT (faixa, amostra real e linhas dos estimadores) |
| Rarefação e extrapolação | iNEXT (números de Hill), com legenda que explica cada linha |
| Diversidade por ponto | Shannon, Simpson, Pielou, números de Hill, teste t de Hutcheson |
| Similaridade | Jaccard, Bray-Curtis, complementaridade, agrupamento (UPGMA), diversidade beta (Whittaker e Baselga) |
| Fitofisionomias | espécies e exclusivas, indivíduos, diversidade (Hill), rarefação de cada ambiente, famílias e similaridade |
| Campanhas | com a coluna Campanha: comparação entre campanhas, NMDS e PERMANOVA |
| Dominância | ranque com o nome de cada espécie |

Cada figura tem **outras formas** de apresentar o mesmo resultado, **títulos editáveis** e a caixa **No relatório**. No fim, o programa monta uma pasta com as figuras numeradas e uma planilha única com todas as tabelas marcadas.

## Instalação (Windows)
1. Baixe a versão mais recente na página **[Releases](../../releases/latest)** → **Source code (zip)**, ou pelo botão verde **Code → Download ZIP** nesta página, e descompacte numa pasta (por exemplo, `Documentos\quadrante`).
   - Já tem uma versão antiga? Descompacte a nova em outra pasta e copie para ela a sua pasta `levantamentos` (e `biblioteca`, se usar). Depois rode o `instalar.cmd` da pasta nova, que refaz o atalho.
2. Dois cliques em **`instalar.cmd`**. Ele confere se o computador tem:
   - o **R** (4.3 ou mais novo);
   - o **Python 3**.

   Se faltar algum, ele oferece instalar pelo `winget` ou abre a página de download. Depois, baixa os pacotes do R (cerca de 150 MB, só na primeira vez) para a pasta `r-pacotes`.
3. Pronto. Para usar: dois cliques em **`abrir_quadrante.cmd`**. O programa abre no navegador; deixe a janela preta aberta enquanto usar.

Para testar, solte a planilha **`exemplo/planilha_exemplo_ficticia.xlsx`** (dados inventados).

## Tutorial em vídeo
O arquivo **[`tutorial/tutorial_quadrante.mp4`](tutorial/tutorial_quadrante.mp4)** (cerca de 6 minutos, vertical, narrado em português) mostra como usar cada parte do programa: abrir, a planilha certa, a conferência, os três botões que recalculam ou redesenham, cada seção do painel, os dados secundários, os pedidos de análise e a pasta do relatório. Ele vem junto no Download ZIP.

A narração usa uma voz sintética (Microsoft Edge, voz neural em português) com efeito de voz rouca.

> Mac e Linux: o programa é R e Python puros e deve funcionar, mas os atalhos `.cmd` são só do Windows. Rode `Rscript R/instalar_pacotes.R` e depois `python3 app/servidor.py`.

## A planilha
Uma aba com **"brutos"** ou **"registros"** no nome, com um registro por linha:

| Ponto | Espécie | Família | Grupo | VO | AV | Total |
|---|---|---|---|---|---|---|
| Ponto 01 | *Rhinella diptycha* | Bufonidae | Anuros | 38 | 2 | 40 |

- **Obrigatórias:** Ponto, Espécie e Total (ou Abundância).
- **Opcionais:**
  - `Família`;
  - `Grupo`: Anuros, Répteis, Aves, Mamíferos, Peixes, Insetos ou outro nome. Cada grupo é analisado à parte, e todos juntos num conjunto;
  - `VO` (vocalização), `AV` (avistamento) e `Sem forma anotada`;
  - `Campanha`.
- **Aba "Pontos"** (opcional):
  - `Fitofisionomia`, com siglas (ex.: MG/VR). Os nomes das siglas ficam em `siglas_fitofisionomias.csv`, que você pode editar;
  - coordenadas, para calcular a distância entre pontos;
  - `Esforço (h)`;
  - `Grupo amostral` (ex.: mata e área aberta), que libera a PERMANOVA.
- **Aba "Dados consolidados"** (opcional): o corretor compara com os registros, e os atributos (endemismo, hábito, sensibilidade, SALVE, IUCN) entram na descritiva.

## Onde ficam os resultados
```
levantamentos\<nome>\
  planilha.xlsx      cópia da planilha enviada (a original não é alterada)
  figuras\           figuras em PNG para o relatório
  tabelas\           planilhas de resultado, uma por tema
  relatorio\         só o que foi marcado "No relatório"
```
Tudo roda no seu computador (endereço 127.0.0.1). As planilhas não são enviadas para lugar nenhum.

## Dados secundários (literatura da região)
Na seção **Dados secundários** do painel, informe o centro (o padrão é o centro dos pontos amostrais) e o raio em km, e salve.
Depois preencha `levantamentos\<nome>\secundarios\dados_secundarios.xlsx` com as obras (artigos, teses, relatórios) da região e as
espécies de cada uma. A aba **Leia-me** explica cada coluna, e `exemplo\dados_secundarios_exemplo_ficticio.xlsx` mostra um exemplo inventado.
Clique em **Rodar de novo**: o Quadrante mostra os descritivos das obras e compara a literatura com o campo (só na literatura,
só no campo, nas duas, campo a confirmar), em figuras e em `tabelas\10_dados_secundarios.xlsx`.
Entram só obras marcadas como lidas e dentro do raio; uma tese e o artigo tirado dela contam uma vez (coluna `obra_derivada_de`).
A pasta `biblioteca\` guarda os PDFs já obtidos, para não buscar a mesma obra de novo.

## Cuidados na leitura
O painel explica, junto de cada análise, o que ela mede, o resultado, como interpretar e os limites. Por exemplo:
- contagens de coro ou de bando são estimativas;
- com poucos pontos, os estimadores ficam instáveis;
- comparações com um ponto por ambiente são só descritivas.

Os resultados ajudam, mas não substituem a leitura de quem fez o campo.

## Pacotes e citação
Versões travadas pela foto do CRAN de 16/11/2026 (Posit Package Manager): vegan, iNEXT, ggplot2, readxl, writexl, jsonlite. Ao usar em relatório, cite o R e os pacotes:
- Oksanen et al., *vegan: Community Ecology Package* (CRAN);
- Hsieh, Ma & Chao 2016, *Methods Ecol. Evol.* 7:1451–1456;
- Chao et al. 2014, *Ecol. Monogr.* 84:45–67;
- Baselga 2010, *Global Ecol. Biogeogr.* 19:134–143;
- Hutcheson 1970, *J. Theor. Biol.* 29:151–154.

## Autor e licença
Autor: Ygor Lima.
Licença MIT (arquivo `LICENSE`): pode usar, mudar e redistribuir, inclusive em trabalho pago, mantendo o aviso de autoria.
