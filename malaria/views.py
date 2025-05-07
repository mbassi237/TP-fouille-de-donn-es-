from django.shortcuts import render
from django.shortcuts import render, redirect, get_object_or_404
from rest_framework import generics
from rest_framework.decorators import api_view
from django.views.decorators.csrf import csrf_exempt
from rest_framework.permissions import IsAuthenticated
from rest_framework.views import APIView
from .serializers import PatientSerializer, FrottisSerializer
from rest_framework.decorators import api_view
from .models import Patient, Frottis, Rapport
import tensorflow as tf
import os 
from rest_framework.response import Response
from django.shortcuts import render, get_object_or_404
from rest_framework import status 
from tensorflow.keras.preprocessing import image
import numpy as np
from tensorflow.keras.models import load_model
from django.core.files.storage import default_storage
from django.http import FileResponse
from io import BytesIO
from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas
from django.contrib import messages
import os
import json
from django.http import HttpResponse, JsonResponse
# Create your views here.



# Enregistrer patient
@api_view(['POST'])
def EnregistrerPatient(request):
    if request.method == 'POST':
        # Créer une instance de serializer avec les données de la requête
        serializer = PatientSerializer(data=request.data)
        
        # Vérifier si les données sont valides
        if serializer.is_valid():
            # Sauvegarder le patient
            patient = serializer.save()
            
            # Générer automatiquement le code_patient
            patient.code_patient = "P" + str(patient.id).zfill(24)
            patient.save()
            
            # Resérialiser avec le code_patient mis à jour
            updated_serializer = PatientSerializer(patient)
            
            # Retourner la réponse avec les données du patient
            return Response({
                'reponse': 'patient bien enregistré',
                'patient': updated_serializer.data
            }, status=status.HTTP_201_CREATED)
        
        # Retourner les erreurs si les données ne sont pas valides
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)


# Supprimer un patient
@api_view(['DELETE'])
def DeletePatient(request, pk):
    try:
        patient = Patient.objects.get(id=pk)
        patient.delete()
        return Response({"message": "Patient supprimé avec succès"}, status=status.HTTP_204_NO_CONTENT)
    except Patient.DoesNotExist:
        return Response({"message": "Patient non trouvé"}, status=status.HTTP_404_NOT_FOUND)


# Afficher la liste des patients
@api_view(['GET'])
def ShowPatient(request):
    patients = Patient.objects.all()
    serializer = PatientSerializer(patients, many=True)
    return Response(serializer.data)


# Modifier un patient
@api_view(['PUT', 'PATCH'])
def UpdatePatient(request, pk):
    try:
        patient = Patient.objects.get(id=pk)
        
        # PATCH pour mise à jour partielle, PUT pour mise à jour complète
        if request.method == 'PATCH':
            serializer = PatientSerializer(patient, data=request.data, partial=True)
        else:  # PUT
            serializer = PatientSerializer(patient, data=request.data)
            
        if serializer.is_valid():
            serializer.save()
            return Response(serializer.data)
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
    except Patient.DoesNotExist:
        return Response({"message": "Patient non trouvé"}, status=status.HTTP_404_NOT_FOUND)



# obtenir un patient spécifique
@api_view(['GET'])
def GetPatientDetail(request, patient_id):
    try:
        patient = Patient.objects.get(id=patient_id)
        serializer = PatientSerializer(patient)
        return Response(serializer.data)
    except Patient.DoesNotExist:
        return Response({"message": "Patient non trouvé"}, status=status.HTTP_404_NOT_FOUND)


# Afficher la liste des patients avec leurs resultats
@api_view(['GET'])
def GetResultsPatient(request):
    patients_analyses = Frottis.objects.select_related('id_patient').all()
    serialzer = FrottisSerializer(patients_analyses, many=True)
    return Response(serialzer.data)



# afficher les diferents reultats d'un patient specifique
@api_view(['GET'])
def GetResultsPatientDetail(request, patient_id):
    try:
        patient_result = Frottis.objects.filter(id_patient=patient_id)
        serializer = FrottisSerializer(patient_result, many=True)
        return Response(serializer.data)
    except Patient.DoesNotExist:
        return Response({"message": "Resultats Patient non trouvé"}, status=status.HTTP_404_NOT_FOUND) 



# fonction pour la detection du plasmodium avec le model CNN
classnames = ['Parasitized', 'Uninfected']
def detection_malaria(image_path, model_path):
    # Chargeons le modele sauvegarde
    model = load_model(model_path)
    # Charger et pretraitons l'image
    img = image.load_img(image_path, target_size=(32, 32))
    img_array = image.img_to_array(img)
    img_array = np.expand_dims(img_array, axis=0)
    img_array /= 255.0 # normaliser l'image
    
    # Predire la classe
    prediction = model.predict(img_array).flatten() # transformation en tableau 1D
    
    # Creation d'un dictionnaire pour les resultats
    results = {
        classnames[i]: round(float(prediction[i]), ndigits=2)
        for i in range(len(classnames))
    }
    
    # Tri par probabilites decroissantes
    #resultats_tries = sorted(results.items(), key=lambda x: x[1], reverse=True)
    return results


# Analyser une image de frottis sanguin d'un patient
@api_view(['POST'])
def AnalyseFrottis(request):
    patients = Patient.objects.all()  # Récupération des patients pour affichage dans le formulaire
    if request.method == 'POST':
        patient_id = request.POST.get('id_patient')  # Récupération de l'ID du patient depuis le formulaire
        patient = get_object_or_404(Patient, id=patient_id)  # Vérification de l'existence du patient

        serializer = FrottisSerializer(data=request.data)
        if serializer.is_valid():
            frottis_instance = serializer.save(id_patient=patient)  # Enregistrement du frottis

            # Lancement de l'analyse avec le modèle CNN C:\Users\Neymar_Jr\Documents\Paludisme_projet\Backend
            image_path = frottis_instance.image.path
            model_path = 'C:/Users/Neymar_Jr/Documents/Paludisme_projet/Backend/shape_classifier.h5'
            resultat_analyse = detection_malaria(image_path, model_path)

            # Mise à jour du frottis avec le statut obtenu
            frottis_instance.status = resultat_analyse
            frottis_instance.save()

            # Retourner les résultats en JSON
            return JsonResponse({
                'status': 'success',
                'resultats': resultat_analyse,
                'id_patient': patient.code_patient,
                'nom_patient': patient.nom,
            })

        else:
            return JsonResponse({
                'status': 'error',
                'errors': serializer.errors
            }, status=status.HTTP_400_BAD_REQUEST)

    return JsonResponse({'status': 'info', 'message': 'Utilisez une requête POST pour analyser un frottis.'})




@api_view(['POST'])
def GenererRapport(request):
    patient_id = request.data.get('id_patient')  # Récupération de l'ID du patient
    patient = get_object_or_404(Patient, id=patient_id)  # Vérification du patient

    # Vérification du frottis le plus récent du patient
    frottis = Frottis.objects.filter(id_patient=patient).order_by('-id').first()
    if not frottis:
        return Response({"error": "Aucun frottis trouvé pour ce patient"}, status=status.HTTP_404_NOT_FOUND)

    buffer = BytesIO()
    p = canvas.Canvas(buffer, pagesize=letter)

    # En-tête du rapport
    p.setFont("Helvetica-Bold", 16)
    p.drawString(200, 770, "Rapport d'analyse Frottis")
    p.line(100, 760, 500, 760)  # Ligne de séparation

    # Informations du patient
    p.setFont("Helvetica", 12)
    p.drawString(100, 730, f"Nom: {patient.nom}")
    p.drawString(100, 710, f"Identifiant: {patient.code_patient}")
    p.drawString(100, 690, f"Sexe: {patient.sexe}")
    p.drawString(100, 670, f"Âge: {patient.age} ans")
    
    # Résultat de l'analyse
    p.setFont("Helvetica-Bold", 14)
    p.drawString(100, 640, "🔍 Résultats de l'analyse :")
    p.setFont("Helvetica", 12)
    p.drawString(100, 620, f"Statut du frottis : {frottis.status}")

    # Finalisation du PDF
    p.showPage()
    p.save()
    
    buffer.seek(0)
    return FileResponse(buffer, as_attachment=True, filename=f"rapport_{patient.nom}.pdf")




def HomePage(request):
    return render(request, 'doameki/master.html')


def PatientFormulaire(request):
    return render(request, 'doameki/patientformulaire.html')


def AuthRegister(request):
    return render(request, 'doameki/auth-register.html')


def AuthLogin(request):
    return render(request, 'doameki/auth-login.html')


def AuthRecover(request):
    return render(request, 'doameki/auth-recover-pw.html')


def Dashboard(request):
    return render(request, 'doameki/dashboard.html')


def AnalyseFrottisSanguin(request):
    return render(request, 'doameki/analyse-frottis.html')