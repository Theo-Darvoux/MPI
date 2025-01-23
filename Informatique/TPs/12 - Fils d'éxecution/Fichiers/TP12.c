#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h> // sleep
#include <semaphore.h>
#include <stdbool.h>

struct requete
{
  int dureeTraitement;
};
typedef struct requete requete;

// Cette fonction simule des connexions entrantes,
// qui ont lieu irrégulièrement et demandent un temps de traitement variable.
void attendreClient(requete *req)
{
  sleep(rand() % 2 + 1);
  req->dureeTraitement = rand() % 15 + 1;
}

void executerRequete(requete *req)
{

  printf("Début traitement requête durée %d par thread %ld\n", req->dureeTraitement, pthread_self());
  sleep(req->dureeTraitement);
  printf("Fin traitement requête durée %d par thread %ld\n", req->dureeTraitement, pthread_self());
}

int main()
{
  srand(time(NULL));
  requete* reqAttente;

  while (1)
  {
    reqAttente = malloc(sizeof(requete));
    attendreClient(reqAttente);
    executerRequete(reqAttente);
    free(reqAttente);
  }
  return 0;
}

