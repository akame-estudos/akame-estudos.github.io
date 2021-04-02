
#colocar funcao para set_wd na localizacao do script (raiz do repositorio)
# setwd("../../")´

diretories <- list.dirs(".", recursive = F)

## ignorar algumas pastas
ignore_folders <- c("./main_page_files",
                    "./.git",
                    "./.github",
                    "./fonts",
                    "./arthur",
                    "./anaih",
                    "./ketlin",
                    "./leps")

#listando projetos
projects <- diretories[!diretories%in%ignore_folders]
projects <- gsub("./", "", projects)

projects_df <- data.frame(projeto = projects, stringsAsFactors = F)

#decricoes dos projetos
acha_descricao <- function(projeto){
  readLines(paste0(projeto,"/data/descricao.md"),
            warn = F)
}

projects_df$descricao <- sapply(projects, acha_descricao)

#ver quem fez o projeto (tem o arquivo index.html)
fez_projeto <- function(pessoa, projeto) {
  "index.html" %in%  list.files(paste0(projeto,"/",pessoa,"/"))  
}

projects_df$arthur <- sapply(projects, fez_projeto, pessoa = "arthur")
projects_df$anaih <- sapply(projects, fez_projeto, pessoa = "anaih")
projects_df$ketlin <- sapply(projects, fez_projeto, pessoa = "ketlin")
projects_df$leps <- sapply(projects, fez_projeto, pessoa = "leps")

#salvar dados como objeto em js
cria_objeto <- function(indice, dados, nome_pessoa){
  
  nome_projeto <- dados$projeto[indice]
  descricao <- dados$descricao[indice]
  
  paste0('{',
            '"projeto":', '"', nome_projeto, '",','\n',
            '"descricao":' , '"', descricao, '",','\n',
            '"link":', '"https://akame-estudos.github.io/', nome_projeto,
                      '/', nome_pessoa,'"',
         '}')
}

cria_array <- function(nome_pessoa){
  dados_array <- projects_df[projects_df[nome_pessoa] == T,]
  
  
  paste0('const ', nome_pessoa,' = [','\n',
         paste(
           sapply(seq_len(nrow(dados_array)),
                  cria_objeto,
                  nome_pessoa = nome_pessoa,
                  dados = dados_array), collapse = ",\n"
         ),
         ']')
  
}

projetos_arthur <- cria_array("arthur")
projetos_anaih <- cria_array("anaih")
projetos_ketlin <- cria_array("ketlin")
projetos_leps <- cria_array("leps")

projetos_geral <- 
  paste(projetos_arthur, "\n",
        projetos_anaih, "\n",
        projetos_ketlin, "\n",
        projetos_leps, "\n"
        )

## salvar em arquivo js
write(projetos_geral, "./projetos.js")

