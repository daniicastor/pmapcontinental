# PMAP MG/ES Continental – Painel Interativo

## Visão Geral

O **PMAP MG/ES Continental** é um painel interativo desenvolvido em **R** para visualização e análise dos dados do Programa de Monitoramento da Atividade Pesqueira (PMAP) na porção continental dos estados de **Minas Gerais** e **Espírito Santo**.

O projeto reúne informações de produção pesqueira, locais de descarga, categorias de pescado, aparelhos de pesca e indicadores socioeconômicos, permitindo explorar os dados por município de forma simples e intuitiva.

---

## Funcionalidades

* Seleção de municípios monitorados.
* Indicadores automáticos de:

  * Captura total (kg);
  * Valor comercializado (R$);
  * Número de descargas monitoradas;
  * Unidades produtivas.
* Mapa municipal com:

  * Município selecionado;
  * Rio Doce;
  * Locais de descarga monitorados;
  * Escala gráfica;
  * Rosa dos ventos.
* Gráficos interativos com:

  * Principais categorias de pescado;
  * Principais aparelhos de pesca;
  * Evolução anual da produção.

---

## Tecnologias Utilizadas

* R
* Shiny
* shinydashboard
* ggplot2
* plotly
* sf
* tidyverse
* ggspatial
* patchwork
* geobr

---

## Estrutura do Projeto

```text
PMAP_Shiny/
│
├── app.R
├── dados.rds
├── MN_continetal.rds
├── rio_doce.rds
├── pontos_sf.rds
├── estados.rds
│
└── www/
    └── logo_pmap.png
```

---

## Como Executar

1. Clone este repositório:

```bash
git clone https://github.com/SEU_USUARIO/PMAP_Shiny.git
```

2. Abra o projeto no RStudio.

3. Instale os pacotes necessários:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "plotly",
  "sf",
  "tidyverse",
  "ggplot2",
  "ggspatial",
  "patchwork"
))
```

4. Execute:

```r
shiny::runApp()
```

ou simplesmente abra o arquivo **app.R** e clique em **Run App**.

---

## Dados

Os dados utilizados neste painel são provenientes do **Programa de Monitoramento da Atividade Pesqueira (PMAP MG/ES Continental)**.

As análises apresentadas possuem finalidade técnica e científica, sendo utilizadas para apoiar a visualização e interpretação dos dados monitorados.

---

## Objetivos

* Facilitar a exploração dos dados pesqueiros por município.
* Apoiar análises espaciais da atividade pesqueira.
* Disponibilizar indicadores de forma rápida e interativa.
* Demonstrar o uso de ferramentas livres para análise geoespacial e visualização de dados.

---

## Desenvolvido por

**Danielle Castor dos Santos**

Analista de Dados • Geoprocessamento • SIG • Ciência de Dados • R • QGIS • SQL

---

## Licença

Este projeto foi desenvolvido para fins técnicos e científicos. Consulte os responsáveis pelo PMAP antes de reutilizar os dados em outros contextos.
