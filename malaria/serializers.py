from rest_framework import serializers 
from .models import Patient, Frottis, Rapport


class PatientSerializer(serializers.ModelSerializer):
    class Meta:
        model = Patient
        fields = '__all__'



class FrottisSerializer(serializers.ModelSerializer):
    nom_patient = serializers.CharField(source='id_patient.nom', read_only=True)
    code_patient = serializers.CharField(source='id_patient.code_patient', read_only=True)
    class Meta:
        model = Frottis
        fields = ['status', 'nom_patient', 'code_patient']
        extra_kwargs = {'status': {'required': False}}



class RapportSerialzer(serializers.ModelSerializer):
    class Meta:
        model = Rapport
        fields = '__all__'