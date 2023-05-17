{\rtf1\ansi\ansicpg1252\cocoartf2709
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 # Zero bounce API - SDK para Pascal\
\
Esta biblioteca es un envoltorio para la API ZeroBounce v2.\
\
Para obtener m\'e1s informaci\'f3n sobre la API, visita https://www.zerobounce.net/docs/.\
\
Para ejecutar esta biblioteca, se requiere la clave de API de Zero Bounce. Consulta [esta gu\'eda](https://www.zerobounce.net/docs/api-dashboard#API_keys_management) para obtener informaci\'f3n sobre c\'f3mo obtener la tuya.\
\
## Unidades del paquete\
\
Los m\'e9todos implementados en esta biblioteca pueden generar `ZbException`.\
\
- __ZbUtility__ - contiene m\'e9todos de utilidad\
    - `ZbSetApiKey` - establece la clave de API que se utilizar\'e1 en todos los m\'e9todos del SDK\
    - `ZbException` - excepci\'f3n que contiene datos sobre el contexto de la respuesta\
- __ZbGenerics__ - contiene m\'e9todos de prop\'f3sito general\
    - `ZbGetCredits` (devuelve un entero) - obtiene los cr\'e9ditos de la cuenta\
    - `ZbGetApiUsage` (devuelve `TApiUsage`) - obtiene el uso de la API de la cuenta (general o espec\'edfico)\
    - `ZbActivityData` (devuelve un entero) - obtiene la cantidad de d\'edas que ha estado activo un buz\'f3n de correo electr\'f3nico\
- __ZbValidation__ - obtiene informaci\'f3n de validaci\'f3n sobre correos electr\'f3nicos\
    - `ZbValidateEmail` (devuelve `TZbValidationResult`) - valida un correo electr\'f3nico\
    - `ZbBatchValidateEmails` (devuelve `TZBBatchValidation`) - valida una lista de correos electr\'f3nicos\
- __ZbBulk__ - validaci\'f3n masiva de correos electr\'f3nicos o puntuaci\'f3n de IA\
    - Validaci\'f3n masiva de correos electr\'f3nicos:\
        - `ZbBulkValidationFileSubmit` (devuelve `TZBFileFeedback`) - env\'eda para validaci\'f3n masiva el contenido de un archivo que contiene correos electr\'f3nicos\
        - `ZbBulkValidationFileStatusCheck` (devuelve `TZBFileStatus`) - verifica el estado de un archivo enviado\
        - `ZbBulkValidationResultFetch` (devuelve `TZBBulkResponse`) - obtiene el resultado de validaci\'f3n de un archivo enviado\
        - `ZbBulkValidationResultDelete` (devuelve `TZBFileFeedback`) - elimina un archivo enviado para validaci\'f3n masiva\
    - Puntuaci\'f3n de IA:\
        - `ZbAiScoringFileSubmit` (devuelve `TZBFileFeedback`) - env\'eda para puntuaci\'f3n de IA el contenido de un archivo que contiene correos electr\'f3nicos\
        - `ZbAiScoringFileStatusCheck` (devuelve `TZBFileStatus`) - verifica el estado de un archivo enviado\
        - `ZbAiScoringResultFetch` (devuelve `TZBBulkResponse`) - obtiene el resultado de validaci\'f3n de un archivo enviado\
        - `ZbAiScoringResultDelete` (devuelve `TZBFileFeedback`) - elimina un archivo enviado para puntuaci\'f3n de IA\
- __ZbStructures__ - contiene las estructuras devueltas por los m\'e9todos enumerados anteriormente\
\
## Uso\
\
### Delphi\
\
Para ejecutar este paquete, debes instalar [Delphi IDE](https://www.embarcadero.com/products/delphi/starter/free-download).\
\
Abre Delphi IDE, selecciona Archivo > Abrir, busca `./packages/ZeroBounce.dpk\
\
` (o `./packages/zb.groupproj`), haz clic en "Abrir paquete".\
\
Para ejecutar los fragmentos de ejemplo o tu propio proyecto usando el paquete Zero Bounce, primero instala el paquete en Delphi:\
- abre el paquete (descrito anteriormente)\
- busca la ventana de proyectos (o CTRL+ALT+F11)\
- haz clic derecho en el proyecto ("ZeroBounce.bpl") > Compilar\
- haz clic derecho en el proyecto > Instalar\
\
Despu\'e9s de instalarlo, los proyectos de ejemplo (`./examples/delphi/Examples.groupproj`) se ejecutar\'e1n correctamente. Si eso a\'fan no sucede, sigue los pasos a continuaci\'f3n.\
\
Para ejecutar el SDK de ZeroBounce con cualquier otro proyecto de Delphi:\
- abre el proyecto deseado\
- Proyecto > Opciones (o SHIFT+CTRL+F11) > Paquetes > Paquetes en tiempo de ejecuci\'f3n\
- habilita "Enlazar con paquetes en tiempo de ejecuci\'f3n"\
- [opcional] haz clic en los tres puntos de "Paquetes en tiempo de ejecuci\'f3n", busca `C:\\Users\\Public\\Documents\\Embarcadero\\Studio\\\{VERSI\'d3N INSTALADA\}\\Bpl`, selecciona "Cualquier archivo (*.*)" en la parte inferior derecha, selecciona el archivo `ZeroBounce.bpl`\
- ahora puedes importar las unidades del proyecto\
\
### Free Pascal\
\
Para ejecutar este paquete, debes instalar [Lazarus IDE](https://www.lazarus-ide.org/).\
\
Abre Lazarus IDE, selecciona Archivo > Abrir, busca `./packages/zerobounce.lpk` (relativo a la ra\'edz del repositorio), haz clic en "Abrir paquete".\
\
Para ejecutar las pruebas unitarias, los fragmentos de ejemplo o tu propio proyecto con el paquete Zero Bounce, primero inst\'e1lalo en Lazarus:\
- abre el paquete (descrito anteriormente)\
- busca la ventana del paquete (con el t\'edtulo "Package ZeroBounce \{versi\'f3n\}")\
- Compilar\
- Utilizar > Instalar\
\
Despu\'e9s de instalarlo, abrir las pruebas (`./tests/unit_tests.lpi`) o los fragmentos de ejemplo (cualquier archivo `*.lpi` de la carpeta `./examples/fpc/`) deber\'eda funcionar correctamente.\
\
Para ejecutar el SDK de ZeroBounce con cualquier otro proyecto de FPC:\
- abre el proyecto deseado\
- Proyecto > Inspector de proyectos > Agregar > Nueva dependencia\
- selecciona "ZeroBounce" de la lista\
- ahora deber\'eda aparecer en "Paquetes requeridos"\
- ahora puedes importar las unidades del proyecto}