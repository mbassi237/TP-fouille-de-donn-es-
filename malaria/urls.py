from django.urls import path
from . import views
from .views import (
    EnregistrerPatient,
    AnalyseFrottis,
    GenererRapport,
    HomePage,
    DeletePatient,
    PatientFormulaire,
    AuthLogin,
    AuthRecover,
    AuthRegister,
    Dashboard,
    ShowPatient,
    GetPatientDetail,
    DeletePatient,
    UpdatePatient,
    AnalyseFrottisSanguin,
    GetResultsPatient,
    GetResultsPatientDetail
    )

urlpatterns = [
    path('patients/', EnregistrerPatient, name='patient-create'),
    path('analysefrottis/', AnalyseFrottisSanguin, name='analyse-frottis'),
    path('rapportpdf/', GenererRapport, name='generer-rapport'),
    path('deletepatients/<int:pk>/', views.DeletePatient,name='delete-patient' ),
    path('home/', views.HomePage,name='home-page' ),
    path('savepatient/', views.PatientFormulaire,name='patient-form' ),
    path('authregister/', views.AuthRegister,name='auth-register' ),
    path('authlogin/', views.AuthLogin,name='auth-login' ),
    path('authrecover/', views.AuthRecover,name='auth-recover' ),
    path('dashboard/', views.Dashboard,name='dashboard' ),
    path('showpatient/', ShowPatient, name='show-patient'),
    path('patientdetail/<int:patient_id>/', GetPatientDetail, name='patient-detail'),
    path('patientdelete/<int:pk>/delete/', DeletePatient, name='delete-patient'),
    path('updatepatient/<int:pk>/update/', UpdatePatient, name='update-patient'),
    path('analyse/', AnalyseFrottis, name='analyse-image'),
    path('getresults/', GetResultsPatient, name='get-results'),
    path('getresultdetail/<int:patient_id>/', GetResultsPatientDetail, name='get-result-detail'),
]