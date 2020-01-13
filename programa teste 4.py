#programa para ativar um ldr
import RPi.GPIO as GPIO
import time
from time import sleep
import csv
from datetime import datetime
import picamera
import cv2
import numpy as np
import skimage
import scipy.ndimage as nd

GPIO.setmode(GPIO.BOARD)

#Definindo o pino do circuito
pin_to_circuit_1 = 11
pin_to_circuit_2 = 13
pin_to_circuit_3 = 16
pin_to_circuit_4 = 18
pin_to_circuit_5 = 22
camera = picamera.PiCamera()

def rc_time (pin_to_circuit):
    count=0
    GPIO.setup(pin_to_circuit,GPIO.OUT)
    GPIO.output(pin_to_circuit, GPIO.LOW)
    time.sleep(.1)

    #Fazendo o pino voltar a ser input
    GPIO.setup(pin_to_circuit,GPIO.IN)

    while ( GPIO.input(pin_to_circuit) == GPIO.LOW):
        count += 1

    return count
    

#Limpeza
with open('Arquivo/saida.csv','a', newline='') as saida:
    saida.write(("Tempo"+","+"LDR1"+","+"LDR2"+","+"LDR3"+","+"LDR4"+","+"LDR5"+","+"med_b"+","+"med_g"+","+"med_r"+","+"std_b"+","+"std_g"+","+"std_r"+","+"ent_b"+","+"ent_g"+","+"ent_r"+","+"med_mediana"+","+"std_mediana"+","+"ent_mediana"+","+"med_LOG"+","+"std_LOG"+","+"ent_LOG"+"\n"))

    try:
    #loop principal
        contador=0
        soma1=0
        soma2=0
        soma3=0
        soma4=0
        soma5=0
        soma_med_b=0
        soma_med_g=0
        soma_med_r=0
        soma_std_b=0
        soma_std_g=0
        soma_std_r=0
        soma_ent_b=0
        soma_ent_g=0
        soma_ent_r=0
        soma_med_mediana=0
        soma_std_mediana=0
        soma_ent_mediana=0
        soma_med_LOG=0
        soma_std_LOG=0
        soma_ent_LOG=0
        while True:
            camera.capture('example.jpg')
            imagem=cv2.imread("example.jpg",1)
            imagem_cinza=cv2.imread("example.jpg",0)
            B=imagem[:,:,0]
            G=imagem[:,:,1]
            R=imagem[:,:,2]
            imagem_mediana=cv2.medianBlur(imagem_cinza, 5)
            imagem_LOG=nd.gaussian_laplace(imagem_cinza, 0.5)
            contador=contador+1
            soma1=soma1+rc_time(pin_to_circuit_1)
            soma2=soma2+rc_time(pin_to_circuit_2)
            soma3=soma3+rc_time(pin_to_circuit_3)
            soma4=soma4+rc_time(pin_to_circuit_4)
            soma5=soma5+rc_time(pin_to_circuit_5)
            soma_med_b=soma_med_b+np.mean(B)
            soma_med_g=soma_med_b+np.mean(G)
            soma_med_r=soma_med_r+np.mean(R)
            soma_std_b=soma_std_b+np.std(B)
            soma_std_g=soma_std_g+np.std(G)
            soma_std_r=soma_std_r+np.std(R)
            
            marg_B=np.histogramdd(np.ravel(B), bins=256)[0]/B.size
            marg_B=list(filter(lambda p: p >0, np.ravel(marg_B)))
            entropia_B=-np.sum(np.multiply(marg_B, np.log2(marg_B)))
            
            soma_ent_b=soma_ent_b+ entropia_B

            marg_G=np.histogramdd(np.ravel(G), bins=256)[0]/G.size
            marg_G=list(filter(lambda p: p >0, np.ravel(marg_G)))
            entropia_G=-np.sum(np.multiply(marg_G, np.log2(marg_G)))
            
            soma_ent_g=soma_ent_g+ entropia_G

            marg_R=np.histogramdd(np.ravel(R), bins=256)[0]/R.size
            marg_R=list(filter(lambda p: p >0, np.ravel(marg_R)))
            entropia_R=-np.sum(np.multiply(marg_R, np.log2(marg_R)))
            
            soma_ent_r=soma_ent_r+ entropia_R

            soma_med_mediana=soma_med_mediana+np.mean(imagem_mediana)
            soma_std_mediana=soma_std_mediana+np.std(imagem_mediana)

            marg_mediana=np.histogramdd(np.ravel(imagem_mediana), bins=256)[0]/imagem_mediana.size
            marg_mediana=list(filter(lambda p: p >0, np.ravel(marg_mediana)))
            entropia_mediana=-np.sum(np.multiply(marg_mediana, np.log2(marg_mediana)))
            
            soma_ent_mediana=soma_ent_mediana+ entropia_mediana
            
            soma_med_LOG=soma_med_LOG+np.mean(imagem_LOG)
            soma_std_LOG=soma_std_LOG+np.std(imagem_LOG)

            marg_LOG=np.histogramdd(np.ravel(imagem_LOG), bins=256)[0]/imagem_LOG.size
            marg_LOG=list(filter(lambda p: p >0, np.ravel(marg_LOG)))
            entropia_LOG=-np.sum(np.multiply(marg_LOG, np.log2(marg_LOG)))
            
            soma_ent_LOG=soma_ent_LOG+ entropia_LOG
            
            if contador==47:
                med1=round(soma1/47,2)
                med2=round(soma2/47,2)
                med3=round(soma3/47,2)
                med4=round(soma4/47,2)
                med5=round(soma5/47,2)
                med_b=round(soma_med_b/47,2)
                med_g=round(soma_med_g/47,2)
                med_r=round(soma_med_r/47,2)
                std_b=round(soma_std_b/47,2)
                std_g=round(soma_std_g/47,2)
                std_r=round(soma_std_r/47,2)
                ent_b=round(soma_ent_b/47,2)
                ent_g=round(soma_ent_g/47,2)
                ent_r=round(soma_ent_r/47,2)
                med_mediana=round(soma_med_mediana/47,2)
                std_mediana=round(soma_std_mediana/47,2)
                ent_mediana=round(soma_ent_mediana/47,2)
                med_LOG=round(soma_med_LOG/47,2)
                std_LOG=round(soma_std_LOG/47,2)
                ent_LOG=round(soma_ent_LOG/47,2)
                contador=0
                soma1=0
                soma2=0
                soma3=0
                soma4=0
                soma5=0
                soma_med_b=0
                soma_med_g=0
                soma_med_r=0
                soma_std_b=0
                soma_std_g=0
                soma_std_r=0
                soma_ent_b=0
                soma_ent_g=0
                soma_ent_r=0
                soma_med_mediana=0
                soma_std_mediana=0
                soma_ent_mediana=0
                soma_med_LOG=0
                soma_std_LOG=0
                soma_ent_LOG=0
                print(med1)
                now=datetime.now()
                saida.write(str(now)+","+str(med1)+","+str(med2)+","+str(med3)+","+str(med4)+","+str(med5)+","+str(med_b)+","+str(med_g)+","+str(med_r)+","+str(std_b)+","+str(std_g)+","+str(std_r)+","+str(ent_b)+","+str(ent_g)+","+str(ent_r)+","+str(med_mediana)+","+str(std_mediana)+","+str(ent_mediana)+","+str(med_LOG)+","+str(std_LOG)+","+str(ent_LOG)+"\n")
    except KeyboardInterrupt:
        pass
    finally:
        GPIO.cleanup()
