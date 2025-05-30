from django.db import models

# Create your models here.


class Patient(models.Model):
    code_patient = models.CharField(max_length=25, blank=True)
    nom = models.CharField(max_length=25)
    sexe = models.CharField(choices={'Masculin': 'Masculin', 'Feminin': 'Feminin'})
    age = models.IntegerField()
    email = models.EmailField(blank=True)
    
    def __str__(self):
        return self.nom



class Frottis(models.Model):
    status = models.CharField(max_length=255)
    image = models.ImageField()
    id_patient = models.ForeignKey(Patient, on_delete=models.CASCADE)
    date = models.DateField(auto_now=True)
    
    def __str__(self):
        return f"{self.status ({self.id_patient})}"



class Rapport(models.Model):
    url_fichier = models.FileField()
    id_frottis = models.ForeignKey(Frottis, on_delete=models.CASCADE)
    
    def __str__(self):
        return self.id_frottis