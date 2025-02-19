#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>


#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct clause 
{
    int var1;  
    int var2;  
    struct clause* next;  
} clause_t;

typedef struct formule 
{
    int nombre_variables;  
    int nombre_clauses;  
    clause_t* expression;  
} formule_t;

// Fonction qui détermine si un littéral (variable ou négation) est satisfait
bool satisfait_litteral(int litteral, bool* valuation)
{
    // Si le littéral est négatif, on vérifie la négation de la variable correspondante
    if (litteral < 0)
    {
        return !valuation[-litteral-1];  // -litteral-1 permet d'accéder à l'indice correct
    }
    // Sinon, on vérifie directement la valeur de la variable
    return valuation[litteral-1];
}

bool satisfait(formule_t phi, bool* valuation)
{
    clause_t* c = phi.expression;  
    bool est_satisfait = true;  
    while (c != NULL)
    {
        bool b1 = satisfait_litteral(c->var1, valuation);  // Vérifie si var1 est satisfait
        bool b2 = satisfait_litteral(c->var2, valuation);  // Vérifie si var2 est satisfait
        est_satisfait = est_satisfait && (b1 || b2);  // La clause est satisfaite si l'un des deux littéraux est vrai
        c = c->next; 
    }
    return est_satisfait;
}

void valuation_suivante(bool* valuation, int n)
{
    for (int i = n-1; i >= 0; i--)
    {
        if (valuation[i])
        {
            valuation[i] = false; 
        }
        else
        {
            valuation[i] = true;  
            return; 
        }
    }
}

bool est_satisfiable(formule_t phi)
{
    // Allocation d'une table de valuation pour stocker les valeurs des variables
    bool* valuation = (bool*) malloc(phi.nombre_variables * sizeof(bool));
    int n = phi.nombre_variables;
    for (int i = 0; i < n ; i++)
    {
        valuation[i] = false;
    }

    bool tous_faux = false;
    // Boucle jusqu'à tester toutes les valuations possibles
    while (!tous_faux)
    {
        if (satisfait(phi, valuation))
        {
            free(valuation);  // Libération de la mémoire
            return true;
        }

        tous_faux = true;
        valuation_suivante(valuation, phi.nombre_variables);
        // Vérifie si toutes les variables sont fausses, auquel cas on arrête
        for (int i = 0; i < n ; i++)
        {
            if (valuation[i])
            {
                tous_faux = false;
            }
        }
    }
    // Libération de la mémoire
    free(valuation);
    return false;
}

typedef struct voisins
{
    int sommet;  
    struct voisins* suivant; 
} voisins_t;

typedef struct graphe
{
    int nombre_sommets;  
    voisins_t** tableau;  
} graphe_t;

void parcours_profondeur(graphe_t g, int n, int s, bool* deja_vus, int* ordre, int* adresse_indice)
{
    deja_vus[s] = true;  
    voisins_t* liste = g.tableau[s];  
    while (liste != NULL)
    {
        if (!deja_vus[liste->sommet])
        {
            parcours_profondeur(g, n, liste->sommet, deja_vus, ordre, adresse_indice);
        }
        liste = liste->suivant;
    }
    ordre[*adresse_indice] = s;  // Ajoute s à la fin de l'ordre
    *adresse_indice = *adresse_indice + 1;  // Incrémente l'indice de la première case libre
}

int* ordre_suffixe(graphe_t g, int n)
{
    bool* deja_vus = (bool*) malloc(sizeof(bool)*n);  
    int* ordre = (int*) malloc(sizeof(int)*n);  
    int indice = 0;
    for (int i = 0; i < n ; i++)
    {
        deja_vus[i] = false;
    }

    // Lancement du parcours en profondeur sur tous les sommets
    for (int s = 0; s < n; s++)
    {
        if (!deja_vus[s])
        {
            parcours_profondeur(g, n, s, deja_vus, ordre, &indice);
        }
    }

    // Libération de la mémoire temporaire
    free(deja_vus);
    return ordre;
}

// Fonction pour ajouter un voisin au début d'une liste de voisins
voisins_t* ajouter(voisins_t* liste, int sommet)
{
    voisins_t* maillon = (voisins_t*) malloc(sizeof(voisins_t));
    maillon->sommet = sommet;
    maillon->suivant = liste;
    return maillon;
}

graphe_t transpose(graphe_t g, int n)
{
    graphe_t nouveau_g;
    nouveau_g.nombre_sommets = n;
    nouveau_g.tableau = (voisins_t**) malloc(sizeof(voisins_t*) * n);
    for (int i = 0; i < n; i++)
    {
        nouveau_g.tableau[i] = NULL;
    }

    for (int s = 0; s < n; s++)
    {
        voisins_t* liste = g.tableau[s];
        while (liste != NULL)
        {
            // Ajoute une arête inversée dans le graphe transposé
            nouveau_g.tableau[liste->sommet] = ajouter(nouveau_g.tableau[liste->sommet], s);
            liste = liste->suivant;
        }
    }

    return nouveau_g;
}

// Parcours en profondeur modifié pour la deuxième phase de l'algorithme de Kosaraju
void second_parcours(graphe_t g, int n, int s, bool* deja_vus, int* cfc, int numero)
{
    deja_vus[s] = true;
    voisins_t* liste = g.tableau[s];

    while (liste != NULL)
    {
        if (!deja_vus[liste->sommet])
        {
            second_parcours(g, n, liste->sommet, deja_vus, cfc, numero);
        }
        liste = liste->suivant;
    }
    // Assigne à chaque sommet son représentant de composante fortement connexe
    cfc[s] = numero;
}

// Libère la mémoire allouée à un graphe
void liberer_graphe(graphe_t g, int n)
{
    for (int s = 0; s < n; s++)
    {
        voisins_t* liste = g.tableau[s];
        while (liste != NULL)
        {
            voisins_t* temp = liste;
            liste = liste->suivant;
            free(temp);  // Libération de chaque maillon de la liste
        }
    }
    free(g.tableau);  // Libération du tableau des voisins
}

// Algorithme de Kosaraju pour trouver les composantes fortement connexes d'un graphe
int* kosaraju(graphe_t g, int n)
{
    // Étape 1 : trouver l'ordre suffixe des sommets dans le graphe original
    int* ordre = ordre_suffixe(g, n);

    // Étape 2 : transposer le graphe
    graphe_t g_t = transpose(g, n);

    // Initialisation des structures pour le deuxième parcours
    bool* deja_vus = (bool*) malloc(sizeof(bool) * n);
    int* cfc = (int*) malloc(sizeof(int) * n);
    for (int i = 0; i < n; i++)
    {
        deja_vus[i] = false;
    }

    int numero = 0;
    // Parcours en profondeur dans l'ordre inverse de l'ordre suffixe
    for (int i = n - 1; i >= 0; i--)
    {
        if (!deja_vus[ordre[i]])
        {
            // Identifie les composantes fortement connexes
            second_parcours(g_t, n, ordre[i], deja_vus, cfc, numero);
            numero+=1;
        }
    }

    // Libération de la mémoire temporaire
    free(ordre);
    free(deja_vus);
    liberer_graphe(g_t, n); 

    return cfc;
}

// Fonction qui étant donné un litteral, calcule le sommet correspondant
int trouver_sommet(int litteral, int n)
{
    // x_i est le sommet i-1
    // non x_i est le sommet n-i où n est le nombre de sommets.
    if (litteral<0)
    {
        return n+litteral;
    }
    return litteral-1;
}

graphe_t g_phi(formule_t phi)
{
    graphe_t g;
    g.nombre_sommets = 2* phi.nombre_variables;
    g.tableau = (voisins_t**) malloc(sizeof(voisins_t)*g.nombre_sommets);
    int n = g.nombre_sommets;
    for (int i = 0; i < n; i++)
    {
        g.tableau[i]=NULL;
    }
    clause_t* c = phi.expression;
    // Pour chaque clause, on ajoute les deux implications nécessaires
    while (c != NULL)
    {
        int a = trouver_sommet(c->var1,n);
        int na = trouver_sommet(-c->var1,n);
        int b = trouver_sommet(c->var2,n);
        int nb = trouver_sommet(-c->var2,n);
        g.tableau[na]=ajouter(g.tableau[na],b);
        g.tableau[nb]=ajouter(g.tableau[nb],a);
        c=c->next;
    }
    return g;
}

bool est_satisfiable_rapide(formule_t phi)
{
    graphe_t g_p = g_phi(phi);
    int* cfc = kosaraju(g_p, g_p.nombre_sommets);

    // Pour chaque variable, on teste si ses littéraux positifs négatifs sont dans une même CFC
    for (int i = 1; i < phi.nombre_variables+1; i++)
    {
        if (cfc[i-1] == cfc[2*phi.nombre_variables-i])
        {
            free(cfc);
            liberer_graphe(g_p, g_p.nombre_sommets);
            return false;
        }
    }
    free(cfc);
    liberer_graphe(g_p,g_p.nombre_sommets);
    return true;
}

// Affiche les voisins de chaque sommet dans un graphe
void afficher_graphe(graphe_t g) 
{
    for (int i = 0; i < g.nombre_sommets; i++) 
    {
        printf("Sommet %d : ", i);
        voisins_t* voisin = g.tableau[i];
        // Parcours des voisins du sommet i
        while (voisin != NULL) 
        {
            printf("%d ", voisin->sommet);
            voisin = voisin->suivant;
        }
        printf("\n");
    }
}

bool* valuation_rapide(formule_t phi)
{
    graphe_t g_p = g_phi(phi);
    int* cfc = kosaraju(g_p, g_p.nombre_sommets);
    bool* valuation = (bool*) malloc(phi.nombre_variables * sizeof(bool));
    bool* assignation = (bool*) malloc(2 * phi.nombre_variables * sizeof(bool));
    for (int i = 0; i < 2 * phi.nombre_variables; i++)
    {
        assignation[i] = false;
    }
    for (int i = 1; i < phi.nombre_variables+1; i++)
    {
        int var = i-1;
        int neg_var = 2 * phi.nombre_variables -i;
        // Si cfc[var] > cfc[neg_var], on est sur de ne pas avoir non x_i qui implique x_i
        if (cfc[var] > cfc[neg_var])
        {
            valuation[i-1] = true;
        }
        else
        {
            valuation[i-1] = false;
        }
    }

    // Libération de la mémoire temporaire
    free(cfc);
    free(assignation);
    liberer_graphe(g_p, g_p.nombre_sommets);

    return valuation;  // Retourne la valuation satisfaisante trouvée
}



int main() 
{
    // Tests de la première partie

    clause_t c1 = {1, 2, NULL}; 
    clause_t c2 = {-2, 3, NULL};
    c1.next = &c2;
    formule_t phi;
    phi.nombre_variables = 3;
    phi.nombre_clauses = 2;
    phi.expression = &c1;
    bool valuation[3] = {true, true, true}; 

    formule_t phi2;
    phi2.nombre_variables = 2;
    phi2.nombre_clauses = 4;
    clause_t d1 = {1, 2, NULL};   // (x1 ∨ x2)
    clause_t d2 = {-1, -2, &d1}; // (¬x1 ∨ ¬x2)
    clause_t d3 = {1, -2, &d2};  // (x1 ∨ ¬x2)
    clause_t d4 = {-1, 2, &d3};  // (¬x1 ∨ x2)
    phi2.expression = &d4;

    // Tester si la formule est satisfaite par cette valuation
    bool resultat = satisfait(phi, valuation);
    printf("Test de la fonction satisfait :\n");
    printf("La formule est-elle satisfaite ? %s\n", resultat ? "Oui" : "Non");

    valuation[2]=false;
    resultat = satisfait(phi, valuation);
    printf("Test de la fonction satisfait :\n");
    printf("La formule est-elle satisfaite ? %s\n", resultat ? "Oui" : "Non");

    // Tester valuation_suivante
    printf("\nTest de la fonction valuation_suivante :\n");
    printf("Valuation initiale : x1 = %d, x2 = %d, x3 = %d\n", valuation[0], valuation[1], valuation[2]);
    valuation_suivante(valuation, 3);
    printf("Valuation suivante : x1 = %d, x2 = %d, x3 = %d\n", valuation[0], valuation[1], valuation[2]);

    // Tester  est_satisfiable
    printf("\nTest de la fonction est_satisfiable :\n");
    // Attendu Oui
    bool est_satisfaite = est_satisfiable(phi);
    printf("La formule est-elle satisfiable ? %s\n", est_satisfaite ? "Oui" : "Non");
    // Attendu Non
    est_satisfaite = est_satisfiable(phi2);
    printf("La formule est-elle satisfiable ? %s\n", est_satisfaite ? "Oui" : "Non");


    // Tests de la deuxième partie

    // Création d'un graphe avec 4 sommets et les arêtes suivantes :
    // 0 -> 1, 1 -> 2, 2 -> 0, 2 -> 3
    graphe_t g;
    g.nombre_sommets = 4;
    g.tableau = (voisins_t**) malloc(4 * sizeof(voisins_t*));
    g.tableau[0] = ajouter(NULL, 1);  // 0 -> 1
    g.tableau[1] = ajouter(NULL, 2);  // 1 -> 2
    g.tableau[2] = ajouter(ajouter(NULL, 3), 0);  // 2 -> 0, 2 -> 3
    g.tableau[3] = NULL;  // 3 n'a pas de voisins

    printf("\nGraphe original :\n");
    afficher_graphe(g);

    // Tester transpose
    printf("\nTest de la fonction transpose :\n");
    graphe_t g_transpose = transpose(g, 4); 
    printf("Graphe transposé :\n");
    afficher_graphe(g_transpose); 
    liberer_graphe(g_transpose, 4);

    // Tester ordre_suffixe
    printf("\nTest de la fonction ordre_suffixe :\n");
    int* ordre = ordre_suffixe(g, 4); 
    printf("Ordre suffixe des sommets :\n");
    for (int i = 0; i < 4; i++) {
        printf("%d ", ordre[i]);
    }
    printf("\n");
    free(ordre);

    // Tester kosaraju
    printf("\nTest de l'algorithme de Kosaraju :\n");
    int* cfc = kosaraju(g, 4);

    // Afficher les composantes fortement connexes
    for (int i = 0; i < 4; i++) 
    {
        printf("Sommet %d est dans la composante %d\n", i, cfc[i]);
    }

    // Libérer la mémoire
    free(cfc);
    for (int i = 0; i < 4; i++) 
    {
        voisins_t* current = g.tableau[i];
        while (current != NULL) {
            voisins_t* tmp = current;
            current = current->suivant;
            free(tmp);
        }
    }
    free(g.tableau);


    // Tests de la troisième partie

    // Tester g_phi
    printf("\nTest 5: Graphe g_phi\n");
    graphe_t g_p = g_phi(phi);
    
    // Affichage du graphe
    afficher_graphe(g_p);

    // Libérer la mémoire allouée pour le graphe
    liberer_graphe(g_p, g_p.nombre_sommets);

    // Tester est_satisfiable_rapide
    est_satisfaite = est_satisfiable_rapide(phi);
    // Attendu Oui
    printf("La formule est-elle satisfiable ? %s\n", est_satisfaite ? "Oui" : "Non");
    // Attendu Non
    est_satisfaite = est_satisfiable_rapide(phi2);
    printf("La formule est-elle satisfiable ? %s\n", est_satisfaite ? "Oui" : "Non");

    // Tester valuation_rapide
    bool * v = valuation_rapide(phi);
    printf("Valuation trouvée : x1 = %d, x2 = %d, x3 = %d\n", v[0], v[1], v[2]);
    free(v);
    return 0;
}



