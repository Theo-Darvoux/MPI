#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

struct pixel {
    int rouge;
    int vert;
    int bleu;
};

typedef struct pixel pixel_t;

struct image {
    int hauteur;
    int longueur;
    pixel_t *tableau;
};

typedef struct image image_t;

float distance_euclide(pixel_t p1, pixel_t p2) 
{
    return 0;
}

int represente_par(pixel_t *representants, int k, pixel_t p) 
{
    return 0;
}

int *represente(image_t img, pixel_t *representants, int k) 
{
    return NULL;
}

pixel_t *initialisation(image_t img, int k) 
{
    return NULL;
}


pixel_t* nouvelle_etape(image_t img, int k, pixel_t* representants) 
{
    return NULL;
}

image_t initialiser(char* nom) 
{
    FILE *fichier = fopen(nom, "r");
    if (fichier == NULL) 
    {
        printf("Impossible d'ouvrir le fichier %s\n", nom);
        exit(EXIT_FAILURE);
    }
    char magic_number[3];
    int largeur, hauteur, valeur_max;
    fscanf(fichier, "%s\n%d %d\n%d\n", magic_number, &largeur, &hauteur, &valeur_max);
    if (strcmp(magic_number, "P3")) 
    {
        printf("Le format du fichier n'est pas P3\n");
        exit(EXIT_FAILURE);
    }
    image_t img = creer(hauteur, largeur);
    for (int i = 0; i < hauteur * largeur; i++) 
    {
        int r, g, b;
        fscanf(fichier, "%d %d %d", &r, &g, &b);
        img.tableau[i].rouge = r;
        img.tableau[i].vert = g;
        img.tableau[i].bleu = b;
    }
    fclose(fichier);
    return img;
}

void ecrire_image(char* destination, image_t img, pixel_t* representants, int k) 
{

}

void variations(char* nom_image, int k) 
{
  
}

int main(){
    srand(time(NULL));
    variations("Images/budapest.ppm",20);
    return 0;
}