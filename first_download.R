pacman::p_load(tidyverse, rio, sidrar, janitor)
options(scipen = 100)



region <- import("banco_com_codigos.xlsx", setclass = "tibble") %>%
  clean_names()
municipio <- str_c("/N6/", paste0(region$codigo, collapse = ","))
cor_2 <- "/c830/all"
sexo_1 <- "/C12564/all"
familiar <- "/C829/46304"
rural <- "/c1/2"


import_db <- function(base){
  get_sidra(api = base) %>%
    tibble() %>%
    janitor::clean_names()
}

sum_all <- function(structure){
  base <- import_db(structure)
  big <- base %>%
    select(-valor, -municipio, -municipio_codigo) %>%
    map_int(n_distinct) %>%
    .[. > 1]
  new_big <- names(big)[!str_detect(names(big), "codigo")]
  if(any(str_detect(colnames(base), "cor_ou_raca"))){
    base <- base %>%
      select(-contains("codigo")) %>%
      mutate(cor_ou_raca_do_produtor = str_replace(cor_ou_raca_do_produtor, "Preta|Parda", "Negra"))
  }
  final <- base %>%
    group_by_if(colnames(.) %in% new_big) %>%
    summarise(total_value = sum(valor, na.rm = T)) %>%
    ungroup()
return(final)
   }
get_totals <- function(base, coluna, genero = F, col_gen = NULL){
  if(genero == F){
    totals <- base %>%
      filter({{coluna}} == "Total") %>%
      select(-{{coluna}}) %>%
      unique()

    return(totals)
  } else{
    origin <- base %>%
      select(-contains('codigo')) %>%
      filter({{coluna}} == "Total")
    man <- origin %>%
      filter({{col_gen}} == "Homens")
    wom <- origin %>%
      filter({{col_gen}} == "Mulheres")
    return(bind_rows(list("man" = man, "woman" = wom)) %>%
             select(-{{coluna}}) %>%
             unique())
  }
}

specifics <- function(original, column_of_interest, function_to_use = NULL, total, string_col){
  
  usecol <- c(colnames({{total}}), string_col)
  
  if(ncol({{total}})>1){
  
  joining <- original %>%
    filter({{function_to_use}}) %>%
    select_if(colnames(.) %in% usecol) %>%
    rename(local_value = total_value)
  
  {{total}} %>%
    left_join(joining) %>%
    mutate(pct = (local_value/total_value)*100)
  } else{
    joining <- original %>%
      filter({{function_to_use}}) %>%
      select_if(colnames(.) %in% usecol) %>%
      rename(local_value = total_value) %>%
      mutate(total_value = total$total_value,
             pct = local_value/total_value*100)
    return(joining)
  }
}

#### pct analfabeto ####

t_alfa <- "/t/6755"
escola <- "/C800/all"

# Analfabetos


alfabeto <- sum_all(str_c(t_alfa, cor_2, municipio, sexo_1, escola, familiar))
totals_alfa <- alfabeto %>%
  get_totals(escolaridade_do_produtor, T, sexo_do_produtor)

analfabetos <- specifics(alfabeto, escolaridade_do_produtor, str_detect(escolaridade_do_produtor, "escrever - não"), totals_alfa,
          "escolaridade_do_produtor") %>%
  arrange(cor_ou_raca_do_produtor)

# Nunca frequentou a escola

alfabeto_2 <- sum_all(str_c(t_alfa, cor_2, municipio, escola, familiar))
totals_alfa_2 <- alfabeto_2 %>%
  get_totals(escolaridade_do_produtor)

nunca_frequentou_escola <- specifics(alfabeto_2, escolaridade_do_produtor, str_detect(escolaridade_do_produtor, "frequentou escola"), totals_alfa_2,
          "escolaridade_do_produtor") %>%
  arrange(cor_ou_raca_do_produtor)


#### Idade ####

t_idade <- "/t/6882"
group_idade <- "/C12771/all"


idade <- sum_all(str_c(t_idade, cor_2, group_idade, municipio, familiar)) %>%
  filter(str_detect(variavel, "Número"))
total_idade <- idade %>%
  get_totals(classe_de_idade_do_produtor)

idade_do_produtor <- specifics(idade, classe_de_idade_do_produtor, cor_ou_raca_do_produtor != "Amarela", 
          total_idade,
          "classe_de_idade_do_produtor") %>%
  arrange(cor_ou_raca_do_produtor) %>%
  filter(cor_ou_raca_do_produtor != "Amarela")

#### Estabelecimentos ####

t_direcao <- "/t/6776"
dirigido <- "/V/9998"


estabelecimento <- sum_all(str_c(t_direcao, cor_2, sexo_1, municipio, dirigido, familiar))

total_estab <- estabelecimento %>%
  get_totals(sexo_do_produtor)

estabelecimentos_por_genero <- specifics(estabelecimento, sexo_do_produtor, cor_ou_raca_do_produtor != "Amarela", 
          total_estab,
          "sexo_do_produtor")


#### Número de estabeleicmentos que obtiveram receitas ####

t_receita <- "/t/6901"
obtencao <- "/V/10006"

n_estab <- estabelecimento %>%
  filter(cor_ou_raca_do_produtor == "Total" &
           sexo_do_produtor == "Total") %>%
  .$total_value


receita <- sum_all(str_c(t_receita, obtencao, municipio, familiar)) %>%
  mutate(estabs = n_estab,
         pct = total_value/estabs)



#### Produção para consumo ####

table_consumo <- "/t/6762"
finalidade <- "/C834/all"
sexo_2 <- "/C12566/all"

consumo <- sum_all(str_c(table_consumo, municipio, finalidade, familiar, sexo_2))
total_consumo <- consumo %>%
  get_totals(sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador)



producao_para_consumo <- specifics(consumo, sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador, total_value != 0, 
          total_consumo,
          "sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador")



#### Produção ####

table_area <- "/t/6759"
atividade <- "/C12517/all"
tamanho <- "/C220/all"

prod <- sum_all(str_c(table_area, cor_2, municipio, familiar, atividade))

total_prod <- prod %>%
  get_totals(grupos_de_atividade_economica)


grupos_atividade_economica <- specifics(prod, grupos_de_atividade_economica, total_value != 0, 
          total_prod,
          "grupos_de_atividade_economica") %>%
  mutate(pct = pct)


#### Acesso financiamento #### 

fin_tab <- "/t/6895"
fin_prog <- "/C12544/all"
financiamento_pct <- "/V/1001990"
financiamento_un <- "/V/1990"


fin_simp <- sum_all(str_c(fin_tab, familiar, municipio, financiamento_un))

estabs <- total_estab %>%
  filter(cor_ou_raca_do_produtor == "Total") %>%
  .$total_value

acesso_financiamento <- fin_simp %>%
  mutate(total = estabs,
         pct = (total_value/total)*100)

fin_cred <- sum_all(str_c(fin_tab, familiar, municipio, fin_prog))
total_cred <- fin_cred %>%
  get_totals(recursos_provenientes_de_programas_governamentais_de_credito)


origem_do_financiamento <- specifics(fin_cred, recursos_provenientes_de_programas_governamentais_de_credito , total_value != 0, 
          total_cred,
          "recursos_provenientes_de_programas_governamentais_de_credito")

#### Orientação técnica ####

t_tec <- "/t/6759"
orienta_tec <- "/C12567/all"

orientacao <- sum_all(str_c(t_tec, orienta_tec, cor_2, municipio, familiar))
total_orientacao <- get_totals(orientacao, origem_da_orientacao_tecnica_recebida)
acesso_orientacao_tecnica <- specifics(orientacao, origem_da_orientacao_tecnica_recebida,
          str_detect(tolower(origem_da_orientacao_tecnica_recebida), "recebe"), total_orientacao, "origem_da_orientacao_tecnica_recebida")


#### Hidrico ####

# Não possuem recurso hídrico


t_rec_hid <- "/t/6861"
tem_rec <- "/V/2324"

rec_hid <- sum_all(str_c(t_rec_hid, familiar, municipio, tem_rec))
nao_possui_recurso_hidrico <- rec_hid %>%
  mutate(total = n_estab,
         pct = (total_value/total)-1)

# % com acesso à água para o uso produtivo (irrigação)

#### Irrigação ####

t_irr <- "/t/6857"
tem_irr <- "/V/2372"


rec_irr <- sum_all(str_c(t_irr, familiar, municipio, tem_irr))
acesso_agua_uso_produtivo <- rec_irr %>%
  mutate(total = n_estab,
         pct = (total_value/total)*100)


#### Cisterna ####


t_cis <- "/t/6860"
tipo <- "/C12482/all"
agua <- "/V/2324"

cisterna %>%
  filter(tolower(tipo_de_recurso_hidrico), "(poços convencionais|cisternas$)")

cisterna <- sum_all(str_c(t_cis, tipo, municipio, familiar))
total_cisterna <- get_totals(cisterna, tipo_de_recurso_hidrico)
tipo_recurso_hidrico <- specifics(cisterna, tipo_de_recurso_hidrico,
          str_detect(tolower(tipo_de_recurso_hidrico), "cisternas$"), 
          total_cisterna, "tipo_de_recurso_hidrico") %>%
  summarise(cisterna = sum(local_value),
            pct_cisterna = cisterna/n_estab,
            local_value = total_value,
            total_value = n_estab,
            pct = local_value/total_value) %>%
  unique()



#### Terra e propriedade ####

# área média por estabelecimentos agropecuários


table_area <- "/t/6759"
tamanho <- "/C220/all"
prop <- "/C218/46503"

area <- sum_all(str_c(table_area, cor_2, municipio, tamanho, familiar))
joined <- count(area, grupos_de_area_total) %>%
  mutate(novos_grupos = str_replace_all(grupos_de_area_total, "\\.", "") %>%
              str_replace_all(",", ".")) %>%
  arrange(novos_grupos) %>%
  mutate(first_num = str_extract(novos_grupos, "[0-9]+[\\.]*[0-9]*"),
         novos_grupos = str_remove(novos_grupos, "[0-9]+[\\.]*[0-9]*"),
         second_num = str_extract(novos_grupos, "[0-9]+[\\.]*[0-9]*")) %>%
  mutate_at(vars(first_num, second_num), as.numeric) %>%
  mutate(mean_val = (first_num + second_num) / 2,
         mean_val = if_else(is.na(mean_val), first_num, mean_val)) %>%
  select(grupos_de_area_total, mean_val)

area <- area %>%
  left_join(joined)

grupos_de_area <- area %>%
  group_by(cor_ou_raca_do_produtor) %>%
  filter(!is.na(total_value) & !is.na(mean_val)) %>%
  summarise(weighted.mean(mean_val, total_value))

# % estabelecimentos com área entre 0.5 e 4 hectares

  
meio_4 <- filter(area, mean_val >= 0.75 & mean_val <= 3.5)
  
total_area <- get_totals(area, grupos_de_area_total) %>%
  select(-mean_val)
entre_05_e_4_hec <- specifics(meio_4, grupos_de_area_total, cor_ou_raca_do_produtor != "Amarela", total_area, "grupos_de_area_total") %>%
  group_by(cor_ou_raca_do_produtor) %>%
  summarise(local_value = sum(local_value),
            total_value = max(total_value)) %>%
  ungroup() %>%
  mutate(pct = local_value/total_value)

inco_cinquenta <- filter(area, mean_val >= 7.5 & mean_val <= 35)

entre_5_50_hec <- specifics(inco_cinquenta, grupos_de_area_total, cor_ou_raca_do_produtor != "Amarela", total_area, "grupos_de_area_total") %>%
  group_by(cor_ou_raca_do_produtor) %>%
  summarise(local_value = sum(local_value),
            total_value = max(total_value)) %>%
  ungroup() %>%
  mutate(pct = local_value/total_value)

# % dos produtores com título de propriedade

proprietario <- sum_all(str_c(table_area, cor_2, municipio, familiar, prop))


pct_produtores_titulo_propriedade <- proprietario %>%
  mutate(total = n_estab,
         pct = total_value/total) %>%
  select(cor_ou_raca_do_produtor, pct)

prop_all <- "/C218/all"
proprietario_geral <- sum_all(str_c(table_area, prop_all, cor_2, municipio, familiar))


total_prop <- get_totals(proprietario_geral, condicao_do_produtor_em_relacao_as_terras)
produtor_proprietario <- specifics(proprietario_geral, condicao_do_produtor_em_relacao_as_terras, total_value != 0, total_prop, "condicao_do_produtor_em_relacao_as_terras") %>%
  filter(condicao_do_produtor_em_relacao_as_terras == "Proprietário(a)")

#### Socio-demográfico 2 ####

# Estabelecimento classificado como AF

t_af <- "/t/6776"
tipo <- "/C829/all"

af <- sum_all(str_c(t_af, tipo, cor_2, municipio))

af_total <- get_totals(af, tipologia)
agricultor_familiar <- specifics(af, tipologia, tipologia == "Agricultura familiar - sim", af_total, "tipologia")
agricultor_nao_familiar <- specifics(af, tipologia, tipologia == "Agricultura familiar - não", af_total, "tipologia")



# Cooperativa ou entidade de classe

t_class <- "/t/6773"
as_classe <- "/C12598/all"

classe <- sum_all(str_c(t_class, as_classe, familiar, municipio)) %>%
  filter(unidade_de_medida == "Unidades")
total_classe <- get_totals(classe, associacao_do_produtor_a_cooperativa_e_ou_a_entidade_de_classe)
cooperativa_ou_entidade_de_classe <- specifics(classe, 
          associacao_do_produtor_a_cooperativa_e_ou_a_entidade_de_classe, 
          total_value != 0, 
          total_classe, 
          "associacao_do_produtor_a_cooperativa_e_ou_a_entidade_de_classe")

# Desocupação

t_desocup <- "/t/6884"
n_pessoal <- "/V/185"
sexo <- "/C2/all"


desocupacao <- sum_all(str_c(t_desocup, n_pessoal, sexo, familiar, municipio))

total_desocupacao <- get_totals(desocupacao, sexo)

pop_total <- "/v/93"
populacao <- sum_all(str_c("/t/1378", municipio, sexo, pop_total, rural)) %>%
  select(sexo, populacao = total_value)
pct_desocupacao <- desocupacao %>%
  left_join(populacao) %>%
  mutate(pct = (total_value/populacao)-1)



# Afazeres domésticos

t_dom <- "/t/7003"
af_dom <- "/v/10177"
ano <- "/P/2019"
piaui <- "/n3/22"
cor <- "/c86/all"
sexo_2 <- "/C2/all"


afazeres <- import_db(str_c("/t/7004", "/v/10178", ano, piaui, cor, sexo_2)) %>%
  select(cor_ou_raca, sexo, valor) %>%
  mutate(cor_ou_raca = if_else(str_detect(cor_ou_raca, "Preta|Parda"), "Negra", cor_ou_raca)) %>%
  group_by(cor_ou_raca, sexo) %>%
  summarise(total_valor = mean(valor))

populacao <- import_db(str_c("/t/6408", piaui, sexo_2, "/v/606", cor, ano)) %>%
  select(cor_ou_raca, sexo, valor) %>%
  mutate(cor_ou_raca = if_else(str_detect(cor_ou_raca, "Preta|Parda"), "Negra", cor_ou_raca)) %>%
  group_by(cor_ou_raca, sexo) %>%
  summarise(total_valor = sum(valor))


pct_afazeres_domesticos <- afazeres %>%
  left_join(populacao)


# Moradores médios

morador <- sum_all(str_c("/t/156", municipio, "/p/2010", "/v/134,619"))
moradores <- import_db(str_c("/t/156", piaui, "/p/2010", "/v/134,619"))

pessoa <- moradores %>%
  filter(str_detect(variavel, "Pessoas")) %>%
  select(n = valor)
media <- moradores %>%
  filter(str_detect(variavel, "Média")) %>%
  select(media = valor)
morador_por_estab <- pessoa %>%
  cbind(media) %>%
  mutate(total= n*media) %>%
  summarise(pct_total = sum(total)/sum(n))


# Esgoto
sanitario <- "/c458/all"
esgoto <- "/C11558/all"
rural <- "/c1/2"


esgotamento <- sum_all(str_c("/t/1394", municipio, esgoto))
total_esgoto <-   get_totals(esgotamento, tipo_de_esgotamento_sanitario)
tipo_de_esgoto <- specifics(esgotamento, tipo_de_esgotamento_sanitario,
          total_value != 0, total_esgoto, "tipo_de_esgotamento_sanitario")


esgotamento <- sum_all(str_c("/t/1394", municipio, esgoto, rural))
total_esgoto <-   get_totals(esgotamento, tipo_de_esgotamento_sanitario)
tipo_de_esgoto_rural <- specifics(esgotamento, tipo_de_esgotamento_sanitario,
          total_value != 0, total_esgoto, "tipo_de_esgotamento_sanitario")


abastecimento <- "/c61/all"

agua <- sum_all(str_c("/t/3217", municipio, abastecimento, rural))
total_agua <- get_totals(agua, forma_de_abastecimento_de_agua)

abastecimento_agua <- specifics(agua, forma_de_abastecimento_de_agua,
          str_detect(variavel, "Domicílios"), total_agua, "forma_de_abastecimento_de_agua")



# Produção e venda

venda <- "/v/10097"
finalidade <- "/C834/all"

vendas <- sum_all(str_c("/t/6961", municipio, familiar, venda))
pct_venda <- vendas %>%
  mutate(estabs = n_estab,
         valor_med = (total_value*1000/estabs)/12)

consumo <- sum_all(str_c("/t/6762", municipio, finalidade, familiar))

n_comercio <- consumo %>%
  filter(str_detect(finalidade_principal_da_producao_agropecuaria_do_estabelecimento, "Comercia")) %>%
  .$total_value

valor_venda <- vendas %>%
  mutate(comercio = n_comercio,
         valor_med = (total_value*1000/comercio)/12)


walk2(list(analfabetos,
           nunca_frequentou_escola,
           idade_do_produtor,
           estabelecimentos_por_genero,
           receita,
           producao_para_consumo,
           grupos_atividade_economica,
           acesso_financiamento,
           origem_do_financiamento,
           acesso_orientacao_tecnica,
           nao_possui_recurso_hidrico,
           acesso_agua_uso_produtivo,
           tipo_recurso_hidrico,
           grupos_de_area,
           entre_05_e_4_hec,
           entre_5_50_hec,
           pct_produtores_titulo_propriedade,
           produtor_proprietario,
           agricultor_familiar,
           agricultor_nao_familiar,
           cooperativa_ou_entidade_de_classe,
           pct_desocupacao,
           pct_afazeres_domesticos,
           morador_por_estab,
           tipo_de_esgoto,
           tipo_de_esgoto_rural,
           abastecimento_agua,
           pct_venda,
           valor_venda),
      c("pct_analfabetos",
        "pct_nunca_frequentou_escola",
        "idade_do_produtor",
        "estabelecimentos",
        "receitas",
        "producao_para_consumo",
        "grupos_atividade_economica",
        "acesso_financiamento",
        "origem_financiamento",
        "acesso_orientacao_tecnica",
        "nao_possui_recurso_hidrico",
        "acesso_agua_para_producao",
        "tipo_de_recurso_hidrico",
        "grupos_de_area",
        "hectares_05-4",
        "hectares_5-50",
        "produtores_com_titulo",
        "proprietarios",
        "agricultor_familiar",
        "nao_agricultor_familiar",
        "cooperativa_ou_entidade_de_classe",
        "pct_desocupacao",
        "pct_afazeres_domesticos",
        "morador_por_estabelecimento",
        "tipo_de_esgoto",
        "tipo_de_esgoto_em_estab_rural",
        "abastecimento_de_agua",
        "pct_da_producao_para_venda",
        "valor_da_venda"), ~write_excel_csv(.x, str_c("final_data/", .y, ".csv")))

