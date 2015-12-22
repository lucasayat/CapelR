
$(document).ready(function() {
  
             var maxi;    
    Shiny.addCustomMessageHandler("max",function(maxid) 
       {
         maxi=parseInt(maxid);
          console.log(maxi)
       });
       
       
            var projs="";    
    Shiny.addCustomMessageHandler("proj",function(projet) 
       {
         projs = projet;
       });  
       
        document.getElementById("info_projet").onclick = function(){
                          var a=  document.getElementById("desproj");
                          var b=  document.getElementById("SAU"); 
                          var c=  document.getElementById("VL"); 
                          var d=  document.getElementById("RDT"); 
                          swal({
                             title: 'Projet en cours :'+"\n"+projs,
                text:a.value+"\n"+ b.value + " ha"+"\n"+ c.value + " VL"+"\n"+c.value*d.value+" Litres de lait produits",animation: "slide-from-top"
                            });
                         }; 

  document.getElementById("dup").onclick = function() 
      {
      var valajout="on";
      var duproj = 0;
      duproj = prompt("Numero de projet à dupliquer :");

       duproj = parseInt(duproj); 
           if(duproj)
             {
               console.log(maxi,duproj);
               if(duproj < 1 || duproj > maxi) {
            swal({title:"Impossible de dupliquer",
                 text:"  le projet"+duproj+" n'existe pas. Vérifier le N°",
               type:"error"});
               valajout="off";
               }else{
        
              var conf=confirm("Le projet"+duproj+" doit avoir été sauvegardé auparavant (bouton -appliquer les              changements du projet- en haut à gauche de l'écran)")
           
                  if(conf==true){
        desproj=prompt("Description du projet:");
        Shiny.onInputChange("ad", valajout);
        Shiny.onInputChange("adproj", duproj);
        Shiny.onInputChange("adesc", desproj);
                }else{
                  return;
               }}
               
             }else{
               return;
             }
       valajout = "off"
      setTimeout(function(){Shiny.onInputChange("ad",valajout)}, 50);
  
      };
  


document.getElementById("sup").onclick = function() 
   {
       var valelim = 0;
       Shiny.onInputChange("supelim", valelim);
       var numelim=prompt("Numero à supprimer?:");
       if (numelim ==null) {
           valelim= 0;  
            }else{  
            numelim=parseInt(numelim); 
            if(numelim <=1 || numelim>maxi) {
            swal({title:"Impossible de supprimer ce projet",
                 text:" Système initial ou qui n'existe pas.\n Le nombre doit ête compris entre 2 et "+" "+maxi,
            type:"error"});
         
             valemin = 0;
               }else{
       sup = confirm("Supprimer la ligne"+numelim+"?");
       if( sup == true) {
          valelim=numelim; 
          }else{
         valelim = 0;   
       }}}
        Shiny.onInputChange("supelim", valelim);
        
       };


   document.getElementById("validall").onclick = function() 
      {
      var valid="on"
      Shiny.onInputChange("val_pro", valid);
             swal({
                 title: projs+" a été modifié.",
                text:"Changements enregistrés. Avant de quitter penser à enregistrer le dossier pour pouvoir le reprendre plus tard." ,  
                type:"warning",
                timer: 1000,
             showConfirmButton: false
                            });
                            
       valid = "off"
      setTimeout(function(){Shiny.onInputChange("val_pro",valid)}, 1000); 
       };

  

   
   document.getElementById("refairot").onclick = function() 
      {
     
      var rot = true;
      Shiny.onInputChange("nouvrot", rot); 
        rot= false;
       setTimeout(function(){Shiny.onInputChange("nouvrot",rot)}, 500);
    
       };


  $(window).bind('beforeunload', function(e) {
    // Your code and validation
    if (confirm) {
        return "Avez-vous sauvegardé le fichier en cours?";
    }
});


});
