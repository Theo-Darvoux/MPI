#include <stdlib.h>
#include <stdio.h>

typedef struct arbre
{
    int clef;
    int valeur;
    int ordre;
    struct arbre* liste_enfants;
    struct arbre* suivant;
}arbre_binomial;

typedef arbre_binomial* tas_binomial;

arbre_binomial* creer_noeud(int clef, int valeur, int ordre)
{
    arbre_binomial* res = malloc(sizeof(arbre_binomial));
    res->clef = clef;
    res->valeur = valeur;
    res->ordre = ordre;
    res->liste_enfants = NULL;
    res->suivant = NULL;
    return res;
}

arbre_binomial* exemple(int n)
{
    arbre_binomial* res = creer_noeud(1, 1, n);
    if (n == 0)
    {
        return res;
    }
    arbre_binomial* dernier_enfant = exemple(n-1);
    res->liste_enfants = dernier_enfant;
    for (int i = n-2; i > -1; i--)
    {
        dernier_enfant->suivant = exemple(i);
        dernier_enfant = dernier_enfant->suivant;
    }
    return res;
}

void liberer(tas_binomial t)
{
    if (t!=NULL)
    {
        liberer(t->liste_enfants);
        liberer(t->suivant);
        free(t);
    }
}

void afficher_arbre(arbre_binomial* a)
{
    printf("%d %d %d %p %p, %p\n",a->clef, a->valeur, a->ordre, a, a->liste_enfants, a->suivant);
    for (arbre_binomial* enfant = a->liste_enfants; enfant != NULL; enfant = enfant->suivant)
    {
        afficher_arbre(enfant);
    }
    printf("%p\n",a);
}

void afficher_tas(tas_binomial t)
{
    if (t!=NULL)
    {
        printf("Nouvel arbre !\n");
        afficher_arbre(t);
        afficher_tas(t->suivant);
    }
}


arbre_binomial* fusion(arbre_binomial* a1, arbre_binomial* a2)
{
    if (a1->clef < a2->clef)
    {
        a2->suivant = a1->liste_enfants;
        a1->liste_enfants = a2;
        a1->ordre++;
        return a1;
    }
    else
    {
        a1->suivant = a2->liste_enfants;
        a2->liste_enfants = a1;
        a2->ordre++;
        return a2;
    }
}

tas_binomial fusionner_doublons(tas_binomial t)
{
    if (t == NULL || t->suivant == NULL)
    {
        return t;
    }
    if (t->ordre != t->suivant->ordre)
    {
        t->suivant = fusionner_doublons(t->suivant);
        return t;
    }
    arbre_binomial* a1 = t;
    arbre_binomial* a2 = t->suivant;
    tas_binomial suite = a2->suivant;
    a2->suivant = NULL;
    a1->suivant = NULL;
    arbre_binomial* a = fusion(a1, a2);
    a->suivant = suite;
    return fusionner_doublons(a);
}

tas_binomial unir_aux(tas_binomial t1, tas_binomial t2)
{
    if (t1 == NULL)
    {
        return t2;
    }
    if (t2 == NULL)
    {
        return t1;
    }
    if (t1->ordre < t2->ordre)
    {
        t1->suivant = unir_aux(t1->suivant, t2);
        return t1;
    }
    else
    {
        t2->suivant = unir_aux(t1, t2->suivant);
        return t2;
    }
}

tas_binomial unir(tas_binomial t1, tas_binomial t2)
{
    return fusionner_doublons(unir_aux(t1,t2));
}

tas_binomial insertion(tas_binomial t1, int clef, int valeur)
{
    arbre_binomial* a = creer_noeud(clef, valeur, 0);
    return unir(t1, a);
}

int trouver_min(tas_binomial t)
{
    int mini_val = t->valeur;
    int mini_clef = t->clef;
    while (t!=NULL)
    {
        if (t->clef<mini_clef)
        {
            mini_clef=t->clef;
            mini_val=t->valeur;
        }
        t=t->suivant;
    }
    return mini_val;
}

tas_binomial retourner(tas_binomial t)
{
    if (t->suivant == NULL)
    {
        return t;
    }
    tas_binomial precedent = t;
    t = t->suivant;
    precedent->suivant = NULL;
    while(t != NULL)
    {
        tas_binomial temp = t->suivant;
        t->suivant = precedent;
        precedent = t;
        t = temp;
    }
    return precedent;
}

tas_binomial extraire_min(tas_binomial t)
{
    int mini_val = trouver_min(t);
    if (t->valeur == mini_val)
    {
        return unir(t->suivant, retourner(t->liste_enfants));
    }
    tas_binomial temp = t->suivant;
    tas_binomial precedent = t;
    while (temp->valeur != mini_val)
    {
        temp=temp->suivant;
    }
    precedent->suivant = temp->suivant;
    temp->suivant = NULL;
    tas_binomial element_retire = temp->liste_enfants;
    free(temp);
    return unir(retourner(element_retire), t);
}


int main()
{
    tas_binomial t = insertion(NULL, 1, 3);
    t = insertion(t, 2, 7);
    t = insertion(t, 4, 6);
    printf("Après 3 insertions :\n");
    afficher_tas(t);
    t = insertion(t, 5, 5);
    printf("Après 4 insertions :\n");
    afficher_tas(t);
    t = insertion(t, 10, 2);
    printf("Après 5 insertions :\n");
    afficher_tas(t);
    printf("Valeur associée à la clef min : %d\n", trouver_min(t));
    t = extraire_min(t);
    printf("Après 1 extraction :\n");
    afficher_tas(t);
    printf("Valeur associée à la clef min : %d\n", trouver_min(t));
    t = extraire_min(t);
    printf("Après 2 extraction :\n");
    afficher_tas(t);
    printf("Valeur associée à la clef min : %d\n", trouver_min(t));
    t = extraire_min(t);
    printf("Après 3 extraction :\n");
    afficher_tas(t);
    liberer(t);
    return 0;
}