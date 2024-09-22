
<!-- README.md is generated from README.Rmd. Please edit that file -->

# refloraR <img src="man/figures/refloraR_hex_sticker.png" align="right" alt="" width="120" />

<!-- badges: start -->
<!-- badges: end -->

An R package for exploring plant specimen collections from [REFLORA
Virtual Herbarium](https://ipt.jbrj.gov.br/reflora) hosted by [Rio de
Janeiro Botanical Garden](https://www.gov.br/jbrj). The package
interacts with the REFLORA Integrated Publishing Toolkit (IPT) by
readily downloading full specimen records for any herbarium in Darwin
Core Format. Also, the package has specific functions to summarize
information and filter specific information by taxonomic or geographical
search.

## The REFLORA programme

The study of Brazil’s flora, renowned as the richest in the world ([BFG
2022](https://doi.org/10.1002/tax.12640)), has a long and storied
history. During the 18th and 19th centuries, European naturalists, along
with a few Brazilian botanists, collected plant specimens while in
Brazil, sending them to herbaria in Europe. The primary aim during this
period was to investigate the flora and its potential uses. Many of
these collections laid the groundwork for the description of new species
or genera (serving as nomenclatural types), and were integral to the
monumental work Flora brasiliensis ([Martius, Eichler & Urban,
1840–1906](https://www.biodiversitylibrary.org/bibliography/454)), which
described over 22,000 species.

In 2010, the Brazilian government launched the REFLORA/CNPq Programme,
with the primary goal of retrieving and making accessible images and
data related to Brazilian plant specimens stored primarily in overseas
herbaria. These resources are now available through the REFLORA Virtual
Herbarium. In 2014, this initiative was strengthened by the
collaboration of SiBBr (Sistema de Informação sobre a Biodiversidade
Brasileira) and the National Forest Inventory (IFN), which expanded the
collections published under the Reflora project.

The [REFLORA Virtual
Herbarium](https://floradobrasil.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ConsultaPublicoHVUC.do)
platform was developed through a partnership between the [Rio de Janeiro
Botanical Garden (JBRJ)](https://www.gov.br/jbrj) and [COPPE (Alberto
Luiz Coimbra Institute for Graduate Studies and Research in Engineering,
UFRJ)](https://coppe.ufrj.br/en/home-en/). JBRJ hosts the system,
overseeing the reception, storage, and publication of the herbarium
images.

Complementing the REFLORA Virtual Herbarium is the Flora platform, first
launched in 2008. This system has evolved through several
stages—including the Lista de Espécies da Flora do Brasil, [Flora do
Brasil 2020](http://doi.org/10.47871/jbrj2021004), and now [Flora e
Funga do Brasil](https://floradobrasil.jbrj.gov.br/consulta/)—and is
also part of the broader REFLORA Programme. Flora e Funga do Brasil
serves as a critical validator for the nomenclature of the REFLORA
Virtual Herbarium’s images, which are continuously refined and enriched
by taxonomists working collaboratively in an online system.

The REFLORA Programme has played a crucial role in Brazil’s success in
meeting the goals of the Global Strategy for Plant Conservation, part of
the Convention on Biological Diversity, for 2010 and 2020. As the
challenges of understanding and studying biodiversity persist, the
REFLORA systems will continue to provide unrestricted access to
high-quality, accurate data on Brazil’s plants, algae, and fungi.  
  

## The REFLORA Virtual Herbarium

The mission to build a virtual herbarium to display the images of
Brazilian plants housed in foreign herbaria was presented by the
[Conselho Nacional de Desenvolvimento Científico e Tecnológico
(CNPq)](https://www.gov.br/cnpq/pt-br) to the Rio de Janeiro Botanical
Garden (JBRJ) in December 2010. The objective was to provide the
capacity to store and display high-quality data regarding Brazil’s Flora
within a public institution. The Reflora Virtual Herbarium is designed
to allow taxonomists to perform procedures similar to those they are
accustomed to doing within physical collections. On this site, they have
access to high-quality images rather than physical specimens, which can
be consulted, re-determined, and typified, among other functionalities.
Curators also have access to specimen determination reports to help
update their physical collections. This innovative system has been
available for study and update by a group of over 900 taxonomists
involved in the List of Species of the Brazilian Flora and Brazilian
Flora 2020 projects.

The initial partners of this initiative were the herbaria
[**K**](https://www.kew.org) (Royal Botanic Gardens, Kew),
[**P**](https://www.mnhn.fr/fr/collection-des-plantes-vasculaires)
(Muséum national d’histoire naturelle, Paris), and the
[**RB**](https://www.gov.br/jbrj) (JBRJ) herbarium. From 2014 onwards,
with the support of [**SiBBr**](https://www.sibbr.gov.br) (Sistema de
Informação sobre a Biodiversidade Brasileira) and
[**IFN**](https://www.gov.br/florestal/pt-br/assuntos/ifn) (Inventário
Florestal Nacional), other European, American, and Brazilian herbaria
were included in this initiative, many of which received equipment and
training to digitize their plant specimens.

Today, there are **81** collections published on this platform,
including:

[**ACAM**](https://ipt.jbrj.gov.br/reflora/resource?r=acam)
(Universidade Estadual da Paraíba),
[**ALCB**](https://ipt.jbrj.gov.br/reflora/resource?r=alcb_herbarium)
(Universidade Federal da Bahia),
[**ASE**](https://ipt.jbrj.gov.br/reflora/resource?r=ase_herbarium)
(Universidade Federal de Sergipe),
[**B**](https://ipt.jbrj.gov.br/reflora/resource?r=b_herb) (Botanischer
Garten und Botanisches Museum Berlin-Dahlem Herbarium),
[**BRBA**](https://ipt.jbrj.gov.br/reflora/resource?r=brba)
(Universidade Federal do Oeste da Bahia),
[**CEN**](https://ipt.jbrj.gov.br/reflora/resource?r=cen) ( Embrapa
Recursos Genéticos e Biotecnologia),
[**CEPEC**](https://ipt.jbrj.gov.br/reflora/resource?r=cepec_herbarium)
(Centro de Pesquisas do Cacau),
[**CESJ**](https://ipt.jbrj.gov.br/reflora/resource?r=cesj)
(Universidade Federal de Juiz de Fora UFJF),
[**CGMS**](https://ipt.jbrj.gov.br/reflora/resource?r=cgms)
(Universidade Federal de Mato Grosso do Sul),
[**COR**](https://ipt.jbrj.gov.br/reflora/resource?r=cor) (Universidade
Federal de Mato Grosso do Sul / Campus Pantanal),
[**CRI**](https://ipt.jbrj.gov.br/reflora/resource?r=cri) (Universidade
do Extremo Sul Catarinense),
[**DVPR**](https://ipt.jbrj.gov.br/reflora/resource?r=dvpr)
(Universidade Tecnológica Federal do Paraná),
[**E**](https://ipt.jbrj.gov.br/reflora/resource?r=e_hv) (Royal Botanic
Garden Edinburgh),
[**EAC**](https://ipt.jbrj.gov.br/reflora/resource?r=eac_herbarium)
(Universidade Federal do Ceará),
[**EBC**](https://ipt.jbrj.gov.br/reflora/resource?r=ebc) (Economic
Botany Collection),
[**ECT**](https://ipt.jbrj.gov.br/reflora/resource?r=ect) (Embrapa Clima
Temperado),
[**ESA**](https://ipt.jbrj.gov.br/reflora/resource?r=esa_herbarium)
(Universidade de São Paulo),
[**EVB**](https://ipt.jbrj.gov.br/reflora/resource?r=evb) (Herbário
Evaldo Buttura),
[**FLOR**](https://ipt.jbrj.gov.br/reflora/resource?r=flor_herbarium)
(Universidade Federal de Santa Catarina),
[**FURB**](https://ipt.jbrj.gov.br/reflora/resource?r=furb_herbarium)
(Universidade Regional de Blumenau),
[**GH**](https://ipt.jbrj.gov.br/reflora/resource?r=gh_herb) (Harvard
University Herbarium),
[**HBR**](https://ipt.jbrj.gov.br/reflora/resource?r=hbr_herbarium)
(Universidade Federal de Santa Catarina),
[**HCF**](https://ipt.jbrj.gov.br/reflora/resource?r=hcf) (Campus Campo
Mourão),
[**HDCF**](https://ipt.jbrj.gov.br/reflora/resource?r=hdcf_herbarium)
(Universidade Federal de Santa Maria),
[**HEPH**](https://ipt.jbrj.gov.br/reflora/resource?r=heph) (Jardim
Botânico de Brasília),
[**HERBAM**](https://ipt.jbrj.gov.br/reflora/resource?r=herbam)
(Amazônia Meridional),
[**HJ**](https://ipt.jbrj.gov.br/reflora/resource?r=hj_herb) (Unidade
Jataí), [**HRCB**](https://ipt.jbrj.gov.br/reflora/resource?r=hrcb)
(Universidade Estadual Paulista UNESP),
[**HSTM**](https://ipt.jbrj.gov.br/reflora/resource?r=hstm)
(Universidade Federal do Oeste do Pará),
[**HTO**](https://ipt.jbrj.gov.br/reflora/resource?r=hto) (UFT” ;),
[**HUCP**](https://ipt.jbrj.gov.br/reflora/resource?r=hucp) (Pontífica
Universidade Católica do Paraná PUC),
[**HUEFS**](https://ipt.jbrj.gov.br/reflora/resource?r=huefs)
(Universidade Estadual de Feira de Santana),
[**HUEM**](https://ipt.jbrj.gov.br/reflora/resource?r=huem)
(Universidade Estadual de Maringá),
[**HUEMG**](https://ipt.jbrj.gov.br/reflora/resource?r=huemg) (Campus
Carangola),
[**HUENF**](https://ipt.jbrj.gov.br/reflora/resource?r=huenf)
(Universidade Estadual do Norte Fluminense),
[**HUFU**](https://ipt.jbrj.gov.br/reflora/resource?r=hufu_herbarium)
(Universidade Federal de Uberlândia),
[**HUNI**](https://ipt.jbrj.gov.br/reflora/resource?r=huni)
(Universidade Federal do Estado do Rio de Janeiro),
[**HURB**](https://ipt.jbrj.gov.br/reflora/resource?r=hurb)
(Universidade do Recôncavo da Bahia),
[**HVASF**](https://ipt.jbrj.gov.br/reflora/resource?r=hvasf) (Fundação
Universidade Federal do Vale do São Francisco),
[**HVAT**](https://ipt.jbrj.gov.br/reflora/resource?r=hvat)
(Universidade do Vale do Taquari),
[**IAN**](https://ipt.jbrj.gov.br/reflora/resource?r=ian) (Embrapa
Amazônia Oriental),
[**IBGE**](https://ipt.jbrj.gov.br/reflora/resource?r=ibge) (Herbário
IBGE), [**ICN**](https://ipt.jbrj.gov.br/reflora/resource?r=icn)
(Universidade Federal do Rio Grande Do Sul),
[**JOI**](https://ipt.jbrj.gov.br/reflora/resource?r=joi) (Univilleda
Região de Joinville),
[**K**](https://ipt.jbrj.gov.br/reflora/resource?r=k_reflora) (Royal
Botanic Gardens, Kew),
[**LUSC**](https://ipt.jbrj.gov.br/reflora/resource?r=lusc)
(Universidade do Estado de Santa Catarina),
[**MAC**](https://ipt.jbrj.gov.br/reflora/resource?r=mac) (Instituto do
Meio Ambiente do Estado de Alagoas),
[**MBM**](https://ipt.jbrj.gov.br/reflora/resource?r=mbm_herbarium)
(Museu Botânico Municipal  Curitiba),
[**MBML**](https://ipt.jbrj.gov.br/reflora/resource?r=mbml) (Museu de
Biologia Mello Leitão),
[**MG**](https://ipt.jbrj.gov.br/reflora/resource?r=mg_herbarium) (Museu
Paraense Emílio Goeldi),
[**MOH**](https://ipt.jbrj.gov.br/reflora/resource?r=moh) ( Missouri
Botanical Garden),
[**MUFAL**](https://ipt.jbrj.gov.br/reflora/resource?r=mufal) (Museu de
História Natural da Universidade Federal de Alagoas),
[**NYH**](https://ipt.jbrj.gov.br/reflora/resource?r=nyh) (The New York
Botanical Garden),
[**P**](https://ipt.jbrj.gov.br/reflora/resource?r=p_reflora) (Muséum
national d’histoire naturelle, Paris),
[**PC**](https://ipt.jbrj.gov.br/reflora/resource?r=pc_herb) (Cryptogamy
Collection at the Muséum National d’Histoire Naturelle),
[**PEL**](https://ipt.jbrj.gov.br/reflora/resource?r=pel) (Universidade
Federal de Pelotas),
[**PMSP**](https://ipt.jbrj.gov.br/reflora/resource?r=pmsp) (Prefeitura
do Município de São Paulo),
[**R**](https://ipt.jbrj.gov.br/reflora/resource?r=r_herbarium) (Museu
Nacional),
[**RB**](https://ipt.jbrj.gov.br/reflora/resource?r=rb_herbarium)
(Jardim Botânico do Rio de Janeiro),
[**RBR**](https://ipt.jbrj.gov.br/reflora/resource?r=rbr) (Universidade
Federal Rural do Rio de Janeiro),
[**REAL**](https://ipt.jbrj.gov.br/reflora/resource?r=real)
(Universidade Federal da Fronteira Sul),
[**RFA**](https://ipt.jbrj.gov.br/reflora/resource?r=rfa) (Universidade
Federal do Rio de Janeiro),
[**RFFP**](https://ipt.jbrj.gov.br/reflora/resource?r=rffp) (Faculdade
de Formação de Professores da Universidade do Estado do Rio de Janeiro),
[**RON**](https://ipt.jbrj.gov.br/reflora/resource?r=ron) (Universidade
Federal de Rondônia),
[**RSPF**](https://ipt.jbrj.gov.br/reflora/resource?r=rspf)
(Universidade de Passo Fundo),
[**S**](https://ipt.jbrj.gov.br/reflora/resource?r=s_reflora)
(Naturhistoriska Riksmuseet),
[**SAMES**](https://ipt.jbrj.gov.br/reflora/resource?r=sames) (Herbário
São Mateus), [**SJRP**](https://ipt.jbrj.gov.br/reflora/resource?r=sjrp)
(Universidade Estadual Paulista Júlio de Mesquita Filho),
[**SLUI**](https://ipt.jbrj.gov.br/reflora/resource?r=slui)
(Universidade Estadual do Maranhão),
[**SOF**](https://ipt.jbrj.gov.br/reflora/resource?r=sof) (Swiss Orchid
Foundation),
[**SPF**](https://ipt.jbrj.gov.br/reflora/resource?r=spf_herbarium)
(Universidade de são Paulo),
[**TEPB**](https://ipt.jbrj.gov.br/reflora/resource?r=tepb)
(Universidade Federal do Piauí),
[**UB**](https://ipt.jbrj.gov.br/reflora/resource?r=ub_herbarium)
(Universidade de Brasília),
[**UFG**](https://ipt.jbrj.gov.br/reflora/resource?r=ufg) (Universidade
Federal de Goiás),
[**UFRN**](https://ipt.jbrj.gov.br/reflora/resource?r=ufrn)
(Universidade Federal do Rio Grande do Norte),
[**UNIP**](https://ipt.jbrj.gov.br/reflora/resource?r=unip)
(Universidade Paulista),
[**UNOP**](https://ipt.jbrj.gov.br/reflora/resource?r=unop)
(Universidade Estadual do Oeste do Paraná),
[**UPCB**](https://ipt.jbrj.gov.br/reflora/resource?r=upcb_herbarium)
(Universidade Federal de Paraná),
[**US**](https://ipt.jbrj.gov.br/reflora/resource?r=us_reflora)
(Smithsonian Institute),
[**VIES**](https://ipt.jbrj.gov.br/reflora/resource?r=vies_herbarium)
(Universidade Federal do Espírito Santo),
[**W**](https://ipt.jbrj.gov.br/reflora/resource?r=w_reflora)
(Naturhistorisches Museum Wien).

There are now 4,602,397 images of specimens available in the REFLORA
Virtual Herbarium. Among them, 157,652 are nomenclatural types and
1,985,843 are georeferenced records.

If you are an herbarium curator and wish to publish images and data from
your collection in REFLORA Virtual Herbarium, send a request to the
contact email **<reflora@jbrj.gov.br>**.  
  

## Installation

You can install the development version of ***refloraR*** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DBOSlab/refloraR")
```

``` r
library(refloraR)
```

  
  

## Usage

A general description of the available main functions
(`reflora_download` and `reflora_summary`) that extract original REFLORA
collections are provided below.  
  

#### *1. `reflora_summary`: Summarizing REFLORA collections*

The following code can be used to extract a summary of all
REFLORA-associated collections, including herbarium acronym, curator’s
email contact, number of records and a direct link to the original
REFLORA Integrated Publishing Toolkit
([IPT](https://ipt.jbrj.gov.br/reflora)).  

``` r
library(refloraR)

summary_df <- reflora_summary(verbose = TRUE,
                              save = TRUE,
                              dir = "reflora_summary")
```

  
By specifying a vector of herbarium acronyms, the user can extract a
summary for just the specific herbarium collection.  

``` r
summary_some_df <- reflora_summary(herbarium = c("ALCB", "RB", "HUEFS", "US", "K"),
                                   verbose = TRUE,
                                   save = TRUE,
                                   dir = "reflora_summary")
```

  
  

#### *2. `reflora_download`: Downloading REFLORA specimen records*

The following code can be used to download original specimen records in
Darwin Core Format and associated metada for all REFLORA collections.  

``` r
library(refloraR)

reflora_download(verbose = TRUE,
                 dir = "reflora_download")
```

  
By specifying a vector of herbarium acronyms, the user can download
specimens records for just the specific herbarium collection.  

``` r
reflora_download(herbarium = c("ALCB", "HUEFS", "RB", "US", "K"),
                 verbose = TRUE,
                 dir = "reflora_download")
```

  
  

## Documentation

A detailed description of the ***refloraR***’s full functionality is
available in different
[articles](https://dboslab.github.io/refloraR/).  
  

## Citation

Cardoso, D. & Caldéron, C. (2024). *refloraR*: An R package for
exploring plant specimen collections from REFLORA Virtual Herbarium.
<https://github.com/dboslab/refloraR>
