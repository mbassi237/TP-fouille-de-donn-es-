from django.contrib import admin
from malaria.models import Rapport, Patient, Frottis
# Register your models here.

admin.site.register(Patient)
admin.site.register(Frottis)
admin.site.register(Rapport)