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

