function uploadImage() {
    var fileInput = document.createElement('input'); 
    fileInput.type = 'file';                                                //Définition du type de l'élément input(ici un fichier)
    fileInput.accept = 'image/*';                                           //Accepter uniquement les fichiers image
    fileInput.style.display = 'none';
    fileInput.addEventListener('change', function() {
        var file = fileInput.files[0];
        if (file) {
            // Afficher le nom de l'image sélectionnée
            document.getElementById('fileName').innerText = 'Nom de fichier sélectionnée : ' + file.name;

            var reader = new FileReader();
            reader.onload = function(event) {
                // Afficher l'image sélectionnée dans l'élément <img> avec l'id "imageDisplay"
                document.getElementById('imageDisplay').src = event.target.result; //contient l'URL de données de l'image sélectionnée, qui est stockée dans l'objet event lorsque le fichier est lu avec 
                document.getElementById('imageDisplay').style.display = 'inline'; // Afficher l'élément <img>
            };
            // Lire le contenu de l'image sélectionnée en tant qu'URL de données
            reader.readAsDataURL(file);
        }
    });

    fileInput.click();
}
//le premier clique
var option1 = document.getElementById('option1');
var formOption1 = document.getElementById('formOption1');

option1.addEventListener('click', () => {

    if (formOption1.style.display != 'none') {
        formOption1.style.display =  'none';
    } else {
        formOption1.style.display =  'block';
    }
});
//le deuXéme clique
var option2 = document.getElementById('option2');
var formOption2 = document.getElementById('formOption2');

option2.addEventListener('click', () => {

    if (formOption2.style.display != 'none') {
        formOption2.style.display =  'none';
    } else {
        formOption2.style.display =  'block';
    }
});

//le 3eme clique
var option3 = document.getElementById('option3');
var formOption3 = document.getElementById('formOption3');

option3.addEventListener('click', () => {

    if (formOption3.style.display != 'none') {
        formOption3.style.display =  'none';
    } else {
        formOption3.style.display =  'block';
    }
});
//le 4eme clique
var option4 = document.getElementById('option4');
var formOption4 = document.getElementById('formOption4');

option4.addEventListener('click', () => {

    if (formOption4.style.display != 'none') {
        formOption4.style.display =  'none';
    } else {
        formOption4.style.display =  'block';
    }
});
//le 5eme clique
var option5 = document.getElementById('option5');
var formOption5 = document.getElementById('formOption5');

option5.addEventListener('click', () => {

    if (formOption5.style.display != 'none') {
        formOption5.style.display =  'none';
    } else {
        formOption5.style.display =  'block';
    }
});
//le 6eme clique
var option6 = document.getElementById('option6');
var formOption6 = document.getElementById('formOption6');

option6.addEventListener('click', () => {

    if (formOption6.style.display != 'none') {
        formOption6.style.display =  'none';
    } else {
        formOption6.style.display =  'block';
    }
});
//le 7eme clique
var option7 = document.getElementById('option7');
var formOption7 = document.getElementById('formOption7');

option7.addEventListener('click', () => {

    if (formOption7.style.display != 'none') {
        formOption7.style.display =  'none';
    } else {
        formOption7.style.display =  'block';
    }
});
//le 8eme clique
var option8 = document.getElementById('option8');
var formOption8 = document.getElementById('formOption8');

option8.addEventListener('click', () => {

    if (formOption8.style.display != 'none') {
        formOption8.style.display =  'none';
    } else {
        formOption8.style.display =  'block';
    }
});
//le 9eme clique
var option9 = document.getElementById('option9');
var formOption9 = document.getElementById('formOption9');

option9.addEventListener('click', () => {

    if (formOption9.style.display != 'none') {
        formOption9.style.display =  'none';
    } else {
        formOption9.style.display =  'block';
    }
});






/*style permet d'accéder aux propriétés CSS de l'élément.
.display est la propriété CSS qui contrôle l'affichage de l'élément.
'inline' est la valeur assignée à la propriété display, ce qui signifie que l'élément est affiché en ligne dans le flux du document.
Ainsi, cette ligne définie la propriété CSS display de l'élément <img> sur "inline", ce qui rend l'image visible dans la page.*/ 