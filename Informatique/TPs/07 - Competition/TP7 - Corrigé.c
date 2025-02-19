#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

#define MOD 1000000007

long long* t_u;

long long* generer_tableau(long long u0, int n)
{
    long long* t = malloc(sizeof(long long)* n);
    long long ut = u0;
    for (int i = 0; i < n; i++) 
    {
        t[i]=ut;
        ut = (900007 * ut) % MOD;
    }
    return t;
}

long long generate_random(long long u0, int t) 
{
    return t_u[t];
}

typedef struct maillon
{
    int valeur;
    struct maillon* suivant;
}maillon;

typedef struct graphe
{
    maillon** tableau;
    int n;
}graphe;

void afficher_graphe(graphe g) 
{
    printf("\nGraphe :\n");
    for (int i = 0; i < g.n; i++) {
        printf("%d:", i);
        maillon* l = g.tableau[i];
        while (l != NULL) {
            printf(" %d", l->valeur);
            l = l->suivant;
        }
        printf("\n");
    }
}

maillon* ajouter_arc(maillon* l, int t)
{
    maillon* nouveau = malloc(sizeof(maillon));
    nouveau->valeur = t;
    nouveau->suivant=l;
    return nouveau;
}

void ajouter_arete(graphe g, int s, int t)
{
    g.tableau[s]=ajouter_arc(g.tableau[s],t);
    g.tableau[t]=ajouter_arc(g.tableau[t],s);
}

void liberer(maillon* l)
{
    if (l!=NULL)
    {
        liberer(l->suivant);
        free(l);
    }
}

void liberer_graphe(graphe g)
{
    int n = g.n;
    for (int i = 0; i < n; i++)
    {
        liberer(g.tableau[i]);
    }
    free(g.tableau);
}


graphe generer_graphe(int n, int m, int k, long long u0) 
{
    graphe g;
    g.n=n+1;
    g.tableau = malloc(sizeof(maillon*)*(n+1));
    for (int x = 0; x < n+1; x++)
    {
        g.tableau[x]=NULL;
    }
    for (int x = 1; x <= n; x++) {
        for (int y = x + 1; y <= n && y <= x + m; y++) 
        {
            long long u_value = generate_random(u0, 17 * y + x);
            if (u_value % k == 0) 
            {
                ajouter_arete(g,x,y);
            }  
        }
    }
    return g;
}

int degre(maillon* l)
{
    int d = 0;
    while (l!=NULL)
    {
        d=d+1;
        l=l->suivant;
    }
    return d;
}

int nombre_aretes(graphe g, int n)
{
    int a = 0;
    for (int i = 0; i < g.n; i++)
    {
        a = a+degre(g.tableau[i]);
    }
    return a/2;
}

void question2(int n, int m, int k, long long u0) 
{
    graphe g = generer_graphe(n, m, k, u0);
    int nb_aretes = nombre_aretes(g,n);
    printf("Nombre d'arêtes pour G(%d, %d, %d): %d\n", n, m, k, nb_aretes);
    liberer_graphe(g);
}

long long calcul_somme_controle(graphe g) 
{
    long long checksum = 0;
    for (long long i = 0; i < g.n; i++) 
    {
        maillon* l = g.tableau[i];
        while (l!=NULL)
        {
            long long j=l->valeur;
            checksum = (checksum + (i+j) * (i+j) + j) % 1000000;
            l=l->suivant;
        }
    }
    return checksum;
}

void question3(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    long long somme_controle = calcul_somme_controle(g);
    printf("Somme de contrôle pour G(%d, %d, %d): %llu\n", n, m, k, somme_controle);
    liberer_graphe(g);
}

void construire_arbre(int u, bool* vus, graphe g, graphe arbre, maillon* l)
{
    vus[u]=true;
    if (l!=NULL)
    {
        construire_arbre(u,vus,g,arbre,l->suivant);
        int v = l->valeur;
        if (!vus[v])
        {
            arbre.tableau[u] = ajouter_arc(arbre.tableau[u], v);
            vus[v] = true;
            construire_arbre(v,vus,g,arbre,g.tableau[v]);
        }
    }
}

graphe generer_arbre(graphe g) 
{
    graphe arbre;
    arbre.n = g.n;
    arbre.tableau = malloc(sizeof(maillon*) * g.n);
    bool* vus = malloc(g.n*sizeof(bool));
    for (int i = 0; i < g.n ; i++) 
    {
        arbre.tableau[i] = NULL;
        vus[i]=false;
    }
    construire_arbre(1, vus, g, arbre,g.tableau[1]);
    free(vus);
    return arbre;
}

void question4(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    graphe a = generer_arbre(g);
    long long somme_controle = calcul_somme_controle(a);
    printf("Somme de contrôle pour A(%d, %d, %d): %llu\n", n, m, k, somme_controle);
    liberer_graphe(g);
    liberer_graphe(a);
}

int algorithme_A1(graphe g) 
{
    bool* domine = malloc(g.n* sizeof(bool));
    for (int i = 0; i < g.n; i++)
    {
        domine[i]=false;
    }
    int taille_dominant = 0;

    for (int i = 1; i < g.n; i++) 
    {
        if (!domine[i]) 
        {
            taille_dominant+=1;
            domine[i] = true;
            maillon* voisin = g.tableau[i];
            while (voisin != NULL) 
            {
                domine[voisin->valeur] = true;
                voisin = voisin->suivant;
            }
        }
    }
    free(domine);
    return taille_dominant;
}

void question5(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    long long domineA1 = algorithme_A1(g);
    printf("Résultat de A1 sur G(%d, %d, %d): %llu\n", n, m, k, domineA1);
    liberer_graphe(g);
}

int parcours(graphe g, int u, bool* vus, maillon* l)
{
    int taille = 0;
    vus[u]=true;
    if (l!=NULL)
    {
        taille=taille+parcours(g,u,vus,l->suivant);
        int v = l->valeur;
        if (!vus[v])
        {
            vus[v] = true;
            taille=taille+parcours(g,v,vus,g.tableau[v]);
        }
    }
    else
    {
        return 1;
    }
    return taille;
}

int algorithme_A2(graphe g)
{
    bool* vus = malloc(g.n*sizeof(bool));
    for (int i = 0; i < g.n ; i++) 
    {
        vus[i]=false;
    }
    int res = 0;
    for (int i = 1 ; i< g.n; i++)
    {
        if (!vus[i])
        {
            int l = parcours(g,i,vus,g.tableau[i]);
            if (l!=1)
            {
                l=l/2;
            }
            res = res + l;
        }
    }
    free(vus);
    return res;
}

void question6(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    long long domineA2 = algorithme_A2(g);
    printf("Résultat de A2 sur G(%d, %d, %d): %llu\n", n, m, k, domineA2);
    liberer_graphe(g);
}

int* construire_score(graphe g)
{
    int* tableau_score = malloc(sizeof(int)*g.n);
    for (int i = 1; i<g.n; i++)
    {
        tableau_score[i]=degre(g.tableau[i])+1;
    }
    return tableau_score;
}

int selectionner(int* tableau_score, int n)
{
    int maxi = 1;
    for (int i = 1; i < n; i++)
    {
        if (tableau_score[i]>tableau_score[maxi])
        {
            maxi = i;
        }
    }
    return maxi;
}

void diminuer_voisins(graphe g, int s, int* tableau_score)
{
    maillon* l = g.tableau[s];
    while (l!=NULL)
    {
        tableau_score[l->valeur]-=1;
        l=l->suivant;
    }
}

int algorithme_A3(graphe g)
{
    int* tableau_score = construire_score(g);
    bool* est_domine = malloc(g.n* sizeof(bool));
    for (int i = 0; i < g.n; i++)
    {
        est_domine[i]=false;
    }
    int nombre = 0;
    int maxi = selectionner(tableau_score, g.n);
    while (tableau_score[maxi] != 0)
    {
        maillon* l = g.tableau[maxi];
        while (l!=NULL)
        {
            if (!est_domine[l->valeur])
            {
                diminuer_voisins(g,l->valeur,tableau_score);
                est_domine[l->valeur]=true;
                tableau_score[l->valeur]-=1;
            }
            l=l->suivant;
        }
        if (!est_domine[maxi])
        {
            diminuer_voisins(g,maxi,tableau_score);
            est_domine[maxi]=true;
            tableau_score[maxi]-=1;
        }
        nombre+=1;
        maxi=selectionner(tableau_score, g.n);
    }
    free(tableau_score);
    free(est_domine);
    return nombre;
}

void question7(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    long long domineA3 = algorithme_A3(g);
    printf("Résultat de A3 sur G(%d, %d, %d): %llu\n", n, m, k, domineA3);
    liberer_graphe(g);
}

void remplir(graphe g, int s, int* nbDominant, int* nbDominantAvec, int* nbDominantSauf)
{
    int sommeDominant = 0;
    int sommeDominantSauf = 0;
    maillon* l = g.tableau[s];
    if (l==NULL)
    {
        nbDominant[s]=1;
        nbDominantAvec[s]=1;
        nbDominantSauf[s]=0;
    }
    else
    {
        while (l!=NULL)
        {
            remplir(g,l->valeur,nbDominant,nbDominantAvec,nbDominantSauf);
            sommeDominant+=nbDominant[l->valeur];
            sommeDominantSauf+=nbDominantSauf[l->valeur];
            l=l->suivant;
        }
        nbDominantAvec[s] = 1+sommeDominantSauf;
        if (nbDominantAvec[s]<sommeDominant)
        {
            nbDominantSauf[s]=nbDominantAvec[s];
        }
        else
        {
            nbDominantSauf[s]=sommeDominant;
        }
        maillon* l = g.tableau[s];
        int mini = nbDominantAvec[l->valeur]+sommeDominant-nbDominant[l->valeur];
        l=l->suivant;
        while (l!=NULL)
        {
            int res = nbDominantAvec[l->valeur]+sommeDominant-nbDominant[l->valeur];
            if (res < mini)
            {
                mini=res;
            }
            l=l->suivant;
        }
        if (mini < nbDominantAvec[s])
        {
            nbDominant[s]=mini;
        }
        else
        {
            nbDominant[s]=nbDominantAvec[s];
        }
    }
}

void question8(int n, int m, int k, long long u0)
{
    graphe g = generer_graphe(n, m, k, u0);
    graphe a = generer_arbre(g);
    int* nbDominant = malloc(g.n*sizeof(int));
    int* nbDominantAvec = malloc(g.n*sizeof(int));
    int* nbDominantSauf = malloc(g.n*sizeof(int));
    remplir(a,1,nbDominant,nbDominantAvec,nbDominantSauf);
    printf("Résultat de la programmation dynamique sur A(%d, %d, %d): %d\n", n, m, k, nbDominant[1]);
    liberer_graphe(a);
    free(nbDominant);
    free(nbDominantAvec);
    free(nbDominantSauf);
    liberer_graphe(g);
}



graphe creer_graphe_exemple() 
{
    graphe g;
    g.n = 6;
    g.tableau = malloc(sizeof(maillon*) * g.n);
    for (int i = 0; i < g.n; i++) 
    {
        g.tableau[i] = NULL;
    }

    ajouter_arete(g, 1, 2);
    ajouter_arete(g, 1, 3);
    ajouter_arete(g, 1, 4);
    ajouter_arete(g, 2, 3);
    ajouter_arete(g, 2, 5);
    ajouter_arete(g, 3, 5);
    return g;
}

int main() {
    long long u0 = 30;  

    t_u = generer_tableau(u0, 2345679);

    printf("\nQuestion 1\n");
    printf("u_1 mod 1000: %llu\n", generate_random(u0, 1) % 1000);
    printf("u_12 mod 1000: %llu\n", generate_random(u0, 12) % 1000);
    printf("u_1234 mod 1000: %llu\n", generate_random(u0, 1234) % 1000);
    printf("u_1234 mod 1000: %llu\n", generate_random(u0, 2345678) % 1000);
    
    printf("\nQuestion 2\n");
    question2(123, 10, 5, u0);
    question2(1234, 10, 5, u0);
    question2(12345, 12, 6, u0);
    question2(54321, 22, 8, u0);

    printf("\nQuestion 3\n");
    question3(123, 10, 5, u0);
    question3(1234, 10, 5, u0);
    question3(12345, 12, 6, u0);
    question3(54321, 22, 8, u0);

    printf("\nQuestion 4\n");
    question4(123, 10, 5, u0);
    question4(1234, 10, 5, u0);
    question4(12345, 12, 6, u0);
    question4(54321, 22, 8, u0);

    printf("\nQuestion 5\n");
    question5(123, 10, 5, u0);
    question5(1234, 10, 5, u0);
    question5(12345, 12, 6, u0);
    question5(54321, 22, 8, u0);

    printf("\nQuestion 6\n");
    question6(123, 10, 5, u0);
    question6(1234, 10, 5, u0);
    question6(12345, 12, 6, u0);
    question6(54321, 22, 8, u0);

    printf("\nQuestion 7\n");
    question7(123, 10, 5, u0);
    question7(1234, 10, 5, u0);
    question7(9876, 1234, 6, u0);
    question7(54321, 22, 8, u0);

    printf("\nQuestion 8\n");
    question8(123, 10, 5, u0);
    question8(1234, 10, 5, u0);
    question8(12345, 12, 6, u0);
    question8(54321, 22, 8, u0);

    free(t_u);
    return 0;
}


