pacman::p_load(tidyverse, rio, sidrar)



import_db <- function(base){
  get_sidra(api = base) %>%
    tibble() %>%
    janitor::clean_names()
}


table <- "/t/6759"
cor_2 <- "/c830/all"
unidade_nacional <- "/N133/22"
tipologia <- "/c829/all"

assistencia <- "/C12567/all"



#### % com acesso a assistência técnica ####
base <- str_c(
  table,
  cor_2,
  unidade_nacional,
  assistencia,
  familiar
)

simples <- str_c(
  table,
  cor_2,
  unidade_nacional,
  familiar
#  assistencia
)
  

#### Benchmark ####

# Total de estabelecimentos familiar

geral <- get_sidra(api = simples) %>%
  tibble() %>%
  janitor::clean_names()

estab_geral <- geral %>%
  filter(cor_ou_raca_do_produtor == "Total") %>%
  .$valor

# Total de estabelecimentos afrodescendentes familiar

af_geral <- geral %>%
  filter(cor_ou_raca_do_produtor == "Parda" | cor_ou_raca_do_produtor == "Preta") %>%
  .$valor %>%
  sum(.)

# Total de estabelecimentos indígenas familiar

in_geral <- geral %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  .$valor


# Acesso assistência técnica

assistencia <- get_sidra(api = base) %>%
  tibble() %>%
  janitor::clean_names()

# Geral

assistencia %>%
  filter(origem_da_orientacao_tecnica_recebida == "Recebe" & cor_ou_raca_do_produtor == "Total") %>%
  select(origem_da_orientacao_tecnica_recebida, cor_ou_raca_do_produtor, valor) %>%
  mutate(total = estab_geral,
         pct = valor/total)

# Indígena

assistencia %>%
  filter(origem_da_orientacao_tecnica_recebida == "Recebe" & cor_ou_raca_do_produtor == "Indígena") %>%
  select(origem_da_orientacao_tecnica_recebida, cor_ou_raca_do_produtor, valor) %>%
  mutate(total = in_geral,
         pct = valor/total)

# Negros

assistencia %>%
  filter(origem_da_orientacao_tecnica_recebida == "Recebe" & (cor_ou_raca_do_produtor == "Preta" | cor_ou_raca_do_produtor == "Parda")) %>%
  select(origem_da_orientacao_tecnica_recebida, cor_ou_raca_do_produtor, valor) %>%
  summarise(valor = sum(valor)) %>%
  mutate(total = af_geral,
         pct = valor/total)


#### Acesso financiamento #### 

fin_tab <- "/t/6895"
fin_prog <- "/C12544/all"
financiamento <- "/V/1001990"

pct_fin <- import_db(str_c(fin_tab, familiar, unidade_nacional, financiamento))

fin <- import_db(str_c(fin_tab, fin_prog, familiar, unidade_nacional))
total_fin <- fin %>%
  filter(recursos_provenientes_de_programas_governamentais_de_credito == "Total") %>%
  .$valor

# Com acesso ao crédito pronaf
fin %>%
  mutate(total = total_fin,
         pct = valor/total) %>%
  select(recursos_provenientes_de_programas_governamentais_de_credito, pct)


#### produção principal ####


table_area <- "/t/6759"
atividade <- "/C12517/all"
tamanho <- "/C220/all"

prod <- import_db(str_c(table_area, cor_2, unidade_nacional, familiar, atividade)) %>%
  select(valor, cor_ou_raca_do_produtor, grupos_de_atividade_economica)

prod_total <- prod[1,1]
prod_in <- prod %>%
  filter(grupos_de_atividade_economica == "Total" & cor_ou_raca_do_produtor == "Indígena") %>%
  .$valor
prod_af <- prod %>%
  filter(grupos_de_atividade_economica == "Total" & (cor_ou_raca_do_produtor == "Preta" | cor_ou_raca_do_produtor == "Parda")) %>%
  summarise(valor = sum(valor)) %>%
  .$valor
  
# total
prod %>%
  filter(cor_ou_raca_do_produtor == "Total") %>%
  mutate(agro = str_detect(grupos_de_atividade_economica, "florestal")) %>%
  group_by(agro) %>%
  mutate(v_agro = sum(valor)) %>%
  ungroup() %>%
  mutate(total = prod_total,
         pct = (valor/total)*100,
         pct_agro = (v_agro/total)*100)

# Indígena
prod %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  mutate(agro = str_detect(grupos_de_atividade_economica, "florestal")) %>%
  group_by(agro) %>%
  mutate(v_agro = sum(valor, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = prod_in,
         pct = (valor/total)*100,
         pct_agro = (v_agro/total)*100)


# Negros
prod %>%
  filter(cor_ou_raca_do_produtor == "Parda" | cor_ou_raca_do_produtor == "Preta") %>%
  group_by(grupos_de_atividade_economica) %>%
  summarise(valor = sum(valor, na.rm = T)) %>%
  ungroup() %>%
  mutate(agro = str_detect(grupos_de_atividade_economica, "florestal")) %>%
  group_by(agro) %>%
  mutate(v_agro = sum(valor, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = prod_af,
         pct = (valor/total)*100,
         pct_agro = (v_agro/total)*100)

#### Produção para consumo ####

table_consumo <- "/t/6762"
finalidade <- "/C834/all"
sexo_adm <- "/C12566/all"


consumo <- import_db(str_c(table_consumo, unidade_nacional, finalidade, familiar, sexo_adm))
# Totais
consumo_total_h <- consumo %>%
  select(finalidade_principal_da_producao_agropecuaria_do_estabelecimento,
         sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador,
         valor) %>%
  filter(finalidade_principal_da_producao_agropecuaria_do_estabelecimento == "Total" & sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador == "Homens") %>%
  .$valor

consumo_total_m <- consumo %>%
  select(finalidade_principal_da_producao_agropecuaria_do_estabelecimento,
         sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador,
         valor) %>%
  filter(finalidade_principal_da_producao_agropecuaria_do_estabelecimento == "Total" & sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador == "Mulheres") %>%
  .$valor

consumo %>%
  select(finalidade_principal_da_producao_agropecuaria_do_estabelecimento,
         sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador,
         valor) %>%
  mutate(total_h = consumo_total_h,
         total_m = consumo_total_m,
         pct_h = valor/total_h,
         pct_m = valor/total_m)


#### Não possuem recursos hídricos ####

t_rec_hid <- "/t/6861"
tem_rec <- "/V/2324"

rec_hid <- import_db(str_c(t_rec_hid, familiar, unidade_nacional, tem_rec))
rec_hid %>%
  select(valor) %>%
  mutate(total = estab_geral,
         pct = (valor/total)-1)

#### Irrigação ####

t_irr <- "/t/6857"
tem_irr <- "/V/2372"


rec_irr <- import_db(str_c(t_irr, familiar, unidade_nacional, tem_irr))
rec_irr %>%
  select(valor) %>%
  mutate(total = estab_geral,
         pct = (valor/total))

#### Cisterna ####


t_cis <- "/t/6860"
tipo <- "/C12482/all"
agua <- "/V/2324"

cisterna <- import_db(str_c(t_cis, tipo, unidade_nacional, familiar))
cisterna %>%
  filter(str_detect(tipo_de_recurso_hidrico, "(poços convencionais|cisternas$)")) %>%
  summarise(cisterna = sum(valor, na.rm = T),
         total = estab_geral,
         pct = (cisterna/total)*100)

agua <- import_db(str_c(t_cis, unidade_nacional, familiar, agua))
cisterna %>%
  filter(tipo_de_recurso_hidrico == "Total") %>%
  summarise(total = estab_geral,
            pct = (valor/total)*100)

cisterna %>%
  filter(str_detect(tipo_de_recurso_hidrico, "cisterna")) %>%
  summarise(cisterna = sum(valor, na.rm = T),
            total = estab_geral,
            pct = (cisterna/total)*100)


#### Terra e propriedade ####


table_area <- "/t/6759"
tamanho <- "/C220/all"
prop <- "/C218/46503"

area <- import_db(str_c(table_area, cor_2, unidade_nacional, tamanho, familiar))


medias <- c(0.05, 0.35, mean(0.5, 1), mean(1, 2), mean(1000, 2500), mean(10,20), mean(10000, 30000), mean(100, 200), mean(2, 3), mean(2500, 10000), mean(20, 50),
            mean(200, 500), mean(3, 4), mean(4, 5), mean(5, 10), mean(50, 100), mean(500, 1000), mean(0, 0.1), 0, 0)
joined <- count(area, grupos_de_area_total) %>%
  cbind(medias)

area <- area %>%
  left_join(joined)

# Indígena

area %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor, medias) %>%
  filter(grupos_de_area_total != "Total") %>%
  filter(!is.na(valor)) %>%
  summarise(media = weighted.mean(medias, valor))

# Afro-descendentes

area %>%
  filter(cor_ou_raca_do_produtor == "Parda" | cor_ou_raca_do_produtor == "Preta") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor, medias) %>%
  group_by(grupos_de_area_total) %>%
  summarise(valor = sum(valor)) %>%
  left_join(joined) %>%
  filter(grupos_de_area_total != "Total") %>%
  filter(!is.na(valor)) %>%
  summarise(media = weighted.mean(medias, valor))

# total
area %>%
  filter(cor_ou_raca_do_produtor == "Total") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor, medias) %>%
  filter(grupos_de_area_total != "Total") %>%
  filter(!is.na(valor)) %>%
  summarise(media = weighted.mean(medias, valor))


# Total estab
total_est <- area %>%
  filter(grupos_de_area_total == "Total" & cor_ou_raca_do_produtor == "Total") %>%
  .$valor
total_in <-  area %>%
  filter(grupos_de_area_total == "Total" & cor_ou_raca_do_produtor == "Indígena") %>%
  .$valor

area %>%
  select(grupos_de_area_total, valor, cor_ou_raca_do_produtor) %>%
  .[5:8,] %>%
  summarise(valor = sum(valor)) %>%
  mutate(total = total_est,
         pct = valor/total_est)

area %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  select(grupos_de_area_total, valor, cor_ou_raca_do_produtor) %>%
  .[5:8,] %>%
  summarise(valor = sum(valor)) %>%
  mutate(total = total_in,
         pct = valor/total)


#### Título de propriedade ####

proprietario <- import_db(str_c(table_area, cor_2, unidade_nacional, familiar, prop))


proprietario %>%
  mutate(total = total_est,
         pct = valor/total) %>%
  select(cor_ou_raca_do_produtor, pct)

proprietario %>%
  mutate(total = total_in,
         pct = valor/total) %>%
  select(cor_ou_raca_do_produtor, pct)


#### Sem atividade economica ####






# Mesmo com indigenas


total_in <- geral %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  summarise(total_indigena = sum(valor))


recebedores_in <- sidra %>%
  filter(tolower(origem_da_orientacao_tecnica_recebida) == "recebe" & (cor_ou_raca_do_produtor == "Indígena")) %>%
  summarise(total_recebe = sum(valor))


recebedores_in$total_recebe/total_in$total_indigena

sidra %>%
  count(cor_ou_raca_do_produtor)



#### Área média por estabelecimentos agropecuários (hectares) ####

table_area <- "/t/6759"
cor_2 <- "/c830/all"
unidade_nacional <- "/N133/22"
tipologia <- "/c829/all"

assistencia <- "/C12567/all"
tamanho <- "/C220/all"

area <- str_c(
  table,
  cor_2,
  unidade_nacional,
  tamanho,
  familiar
)


sidra_area <- get_sidra(api = area) %>%
  tibble() %>%
  janitor::clean_names()


areas <- c(0.05, 0.15, 0.35, 0.75, 1.5, 2.5, 3.5, 4.5, 5.5, 15, 35, 75, 150, 350, 750, 1750, 6000, 10000, 0)

sidra_area %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor) %>%
  filter(grupos_de_area_total != "Total") %>%
  cbind(areas) %>%
  tibble() %>%
  filter(!is.na(valor)) %>%
  summarise(weighted.mean(areas, valor))

# Afro-descendentes

areas <- c(0.05, 0.15, 0.35, 0.75, 1.5, 2.5, 3.5, 4.5, 5.5, 15, 35, 75, 150, 350, 750, 1750, 6000, 10000, 0)

sidra_area %>%
  filter(cor_ou_raca_do_produtor == "Parda" | cor_ou_raca_do_produtor == "Preta") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor) %>%
  filter(grupos_de_area_total != "Total") %>%
  cbind(areas) %>%
  tibble() %>%
  filter(!is.na(valor)) %>%
  summarise(weighted.mean(areas, valor))

# total




sidra_area %>%
  filter(cor_ou_raca_do_produtor == "Total") %>%
  select(cor_ou_raca_do_produtor, grupos_de_area_total, valor) %>%
  filter(grupos_de_area_total != "Total") %>%
  cbind(areas) %>%
  tibble() %>%
  filter(!is.na(valor)) %>%
  summarise(weighted.mean(areas, valor))


#### Produção ####


table_area <- "/t/6759"
cor_2 <- "/c830/all"
unidade_nacional <- "/N133/22"
tipologia <- "/c829/all"
atividade <- "/C12517/all"
assistencia <- "/C12567/all"
tamanho <- "/C220/all"

prod_base <- str_c(table_area, cor_2, unidade_nacional, atividade)
total_atividade <- get_sidra(api = prod_base) %>%
  tibble() %>%
  janitor::clean_names()


total_in$total_indigena

total_atividade %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  select(grupos_de_atividade_economica, valor) %>%
  mutate(total = total_in$total_indigena,
         pct = valor/total)

total_atividade %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  select(grupos_de_atividade_economica, valor) %>%
  filter(str_detect(grupos_de_atividade_economica, "Produção florestal")) %>%
  mutate(total = 303,
         pct = valor/total)

total_atividade %>%
  filter(cor_ou_raca_do_produtor == "Preta" | cor_ou_raca_do_produtor == "Parda") %>%
  group_by(grupos_de_atividade_economica) %>%
  summarise(valor = sum(valor)) %>%
  mutate(total = total_afro$total_negros,
         pct = valor/total)

total_atividade %>%
  filter(cor_ou_raca_do_produtor == "Preta" | cor_ou_raca_do_produtor == "Parda") %>%
  group_by(grupos_de_atividade_economica) %>%
  summarise(valor = sum(valor)) %>%
  filter(str_detect(grupos_de_atividade_economica, "Produção florestal")) %>%
  summarise(valor = sum(valor),
            grupo = "lavrouras agroflorestais") %>%
  mutate(total = 157785,
         pct = valor/total)

#### Produção para consumo ####

table_consumo <- "/t/6762"
cor_3 <- "/c86/all"
finalidade <- "/C834/all"
unidade_nacional <- "/N133/22"
tipologia <- "/c829/all"
atividade <- "/C12517/all"
assistencia <- "/C12567/all"
tamanho <- "/C220/all"
sexo <- "/C2/all"
sexo_adm <- "/C12566/all"


consumo <- get_sidra(api = str_c(table_consumo, unidade_nacional, finalidade, familiar)) %>%
  tibble() %>%
  janitor::clean_names()

consumo %>%
  View()

total_consumo <- consumo %>%
  filter(finalidade_principal_da_producao_agropecuaria_do_estabelecimento == "Total")

consumo %>%
  filter(finalidade_principal_da_producao_agropecuaria_do_estabelecimento != "Total") %>%
  mutate(total = total_consumo$valor,
         pct = valor/total) %>%
  select(finalidade_principal_da_producao_agropecuaria_do_estabelecimento, pct, valor, total)


consumo_genero <- get_sidra(api = str_c(table_consumo, unidade_nacional, finalidade, sexo_adm, familiar)) %>%
  tibble() %>%
  janitor::clean_names()

total_consumo_proprio <- consumo %>%
  filter(str_detect(finalidade_principal_da_producao_agropecuaria_do_estabelecimento, "Consumo próprio"))

consumo_genero %>%
  filter(finalidade_principal_da_producao_agropecuaria_do_estabelecimento != "Total") %>%
  mutate(total = total_consumo_proprio$valor,
         pct = valor/total) %>%
  select(finalidade_principal_da_producao_agropecuaria_do_estabelecimento, sexo_da_pessoa_que_dirige_o_estabelecimento_produtor_ou_administrador,  pct, valor, total)



sa <- rio::import("05_unidades_da_federacao.xls", setclass = "tibble")
sa %>%
  janitor::clean_names() %>%
  select(1, 4) %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  filter(grandes_regioes_e_unidades_da_federacao == "Pará")

get_sidra(api = "http://api.sidra.ibge.gov.br/values/t/418/n103/all/v/all/p/all/c216/0,3808,3811,3815,3818,3821,3825,3829,3836,3840,3843,3846,3850,3853,3856,3859,3864,104327,104330/c201/6196/d/v180%203?formato=json")



#### Obtenção de renda ####

table_estabelecimento <- "/t/6901"
cor_3 <- "/c86/all"
finalidade <- "/C834/all"
unidade_nacional <- "/N133/22"
tipologia <- "/c829/all"
atividade <- "/C12517/all"
assistencia <- "/C12567/all"
tamanho <- "/C220/all"
sexo <- "/C2/all"
sexo_adm <- "/C12566/all"


total <- get_sidra(api = str_c("/t/6897", unidade_nacional, "/V/40", familiar)) %>%
  tibble() %>%
  janitor::clean_names()

renda <- get_sidra(api = str_c(table_estabelecimento, "/V/all", unidade_nacional, familiar)) %>%
  tibble() %>%
  janitor::clean_names()

estab_renda <- renda %>%
  filter(variavel_codigo == 10006)
  
estab_renda %>%
  mutate(total = total$valor) %>%
  summarise(pct = valor/total)

renda %>%
  filter(variavel_codigo == 10007) %>%
  mutate(locais_renda = estab_renda$valor) %>%
  summarise(med_valor = valor/locais_renda)


#### Recursos hidricos ####










t_hid <- "/t/6860"
tipo <- "/C12482/all"

hidrico <- get_sidra(api = str_c(t_hid, tipo, unidade_nacional)) %>%
  tibble() %>%
  janitor::clean_names()


total_hidrico <- hidrico %>%
  filter(tipo_de_recurso_hidrico == "Total")

hidrico %>%
  filter(str_detect(tipo_de_recurso_hidrico, "cisterna")) %>%
  mutate(cisterna = sum(valor),
         total = total$valor,
         pct = cisterna/total) %>%
  select(cisterna, total, pct)

#### Alfabetismo ####

escolar <- "/C800/all"

alfabeto <- get_sidra(api = str_c("/t/6755", unidade_nacional, cor_2, escolar)) %>%
  tibble() %>%
  janitor::clean_names()  %>%
  select(cor_ou_raca_do_produtor, valor, variavel, escolaridade_do_produtor)


alfabeto %>%
  mutate(total = 207787,
         pct = valor/total) %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  mutate(total_ind = 303,
         pct_ind = valor/total_ind)



#### Base familiar ####

familiar <- "/C829/46304"
genero <- "/C12564/all"

f_alfa <- get_sidra(api = str_c("/t/6755", unidade_nacional, cor_2, escolar, familiar, genero)) %>%
  tibble() %>%
  janitor::clean_names()  %>%
  select(cor_ou_raca_do_produtor, valor, variavel, escolaridade_do_produtor, sexo_do_produtor)


# total
f_alfa %>%
  mutate(total_h = 129390,
         total_m = 38137,
         pct_h = valor/total_h,
         pct_m = valor/total_m) %>%
  filter(cor_ou_raca_do_produtor != "Total")

# negros
f_alfa %>%
  filter(cor_ou_raca_do_produtor == "Preta" | cor_ou_raca_do_produtor == "Parda") %>%
  group_by(sexo_do_produtor, escolaridade_do_produtor) %>%
  summarise(valor = sum(valor)) %>%
  filter(escolaridade_do_produtor == "Total" | str_detect(escolaridade_do_produtor, "Sabe ler e escrever")) %>%
  mutate(total_h = 98407,
         total_m = 29500,
         pct_h = valor/total_h,
         pct_m = valor/total_m)

# indigenas
f_alfa %>%
  filter(cor_ou_raca_do_produtor == "Indígena") %>%
  mutate(total_h = 162,
         total_m = 67,
         pct_h = valor/total_h,
         pct_m = valor/total_m)
