var article = document.querySelector("#projects");

// funcao para criar os botoes dos projetos
create_project = function(projecData){
  var project = document.createElement("div");
  project.className = "project-wrapper";
  project.onclick = function(){
    window
    .open(projecData.link, '_blank')
    .focus();
  };

  var title = document.createElement("h3");
  var titleText = document.createTextNode(projecData.projeto);
  var paragraph = document.createElement("P");                   
  var paragraphText = document.createTextNode(projecData.descricao);   

  title.appendChild(titleText);
  paragraph.appendChild(paragraphText);

  project.appendChild(title);
  project.appendChild(paragraph); 

  article.appendChild(project);
} ;

for (let i = 0; i < ketlin.length; i++) {
  create_project(ketlin[i]);
}
