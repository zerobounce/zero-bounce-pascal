# Zero bounce API - SDK para Pascal

Esta biblioteca es un envoltorio para la API ZeroBounce v2.

Para obtener más información sobre la API, visita https://www.zerobounce.net/docs/.

Para ejecutar esta biblioteca, se requiere la clave de API de Zero Bounce. Consulta [esta guía](https://www.zerobounce.net/docs/api-dashboard#API_keys_management) para obtener información sobre cómo obtener la tuya.

## Unidades del paquete

Los métodos implementados en esta biblioteca pueden generar `ZbException`.

- __ZbUtility__ - contiene métodos de utilidad
    - `ZbSetApiKey` - establece la clave de API que se utilizará en todos los métodos del SDK
    - `ZbException` - excepción que contiene datos sobre el contexto de la respuesta
- __ZbGenerics__ - contiene métodos de propósito general
    - `ZbGetCredits` (devuelve un entero) - obtiene los créditos de la cuenta
    - `ZbGetApiUsage` (devuelve `TApiUsage`) - obtiene el uso de la API de la cuenta (general o específico)
    - `ZbActivityData` (devuelve un entero) - obtiene la cantidad de días que ha estado activo un buzón de correo electrónico
- __ZbValidation__ - obtiene información de validación sobre correos electrónicos
    - `ZbValidateEmail` (devuelve `TZbValidationResult`) - valida un correo electrónico
    - `ZbBatchValidateEmails` (devuelve `TZBBatchValidation`) - valida una lista de correos electrónicos
- __ZbBulk__ - validación masiva de correos electrónicos o puntuación de IA
    - Validación masiva de correos electrónicos:
        - `ZbBulkValidationFileSubmit` (devuelve `TZBFileFeedback`) - envía para validación masiva el contenido de un archivo que contiene correos electrónicos
        - `ZbBulkValidationFileStatusCheck` (devuelve `TZBFileStatus`) - verifica el estado de un archivo enviado
        - `ZbBulkValidationResultFetch` (devuelve `TZBBulkResponse`) - obtiene el resultado de validación de un archivo enviado
        - `ZbBulkValidationResultDelete` (devuelve `TZBFileFeedback`) - elimina un archivo enviado para validación masiva
    - Puntuación de IA:
        - `ZbAiScoringFileSubmit` (devuelve `TZBFileFeedback`) - envía para puntuación de IA el contenido de un archivo que contiene correos electrónicos
        - `ZbAiScoringFileStatusCheck` (devuelve `TZBFileStatus`) - verifica el estado de un archivo enviado
        - `ZbAiScoringResultFetch` (devuelve `TZBBulkResponse`) - obtiene el resultado de validación de un archivo enviado
        - `ZbAiScoringResultDelete` (devuelve `TZBFileFeedback`) - elimina un archivo enviado para puntuación de IA
- __ZbStructures__ - contiene las estructuras devueltas por los métodos enumerados anteriormente

## Uso

### Delphi

Para ejecutar este paquete, debes instalar [Delphi IDE](https://www.embarcadero.com/products/delphi/starter/free-download).

Abre Delphi IDE, selecciona Archivo > Abrir, busca `./packages/ZeroBounce.dpk

` (o `./packages/zb.groupproj`), haz clic en "Abrir paquete".

Para ejecutar los fragmentos de ejemplo o tu propio proyecto usando el paquete Zero Bounce, primero instala el paquete en Delphi:
- abre el paquete (descrito anteriormente)
- busca la ventana de proyectos (o CTRL+ALT+F11)
- haz clic derecho en el proyecto ("ZeroBounce.bpl") > Compilar
- haz clic derecho en el proyecto > Instalar

Después de instalarlo, los proyectos de ejemplo (`./examples/delphi/Examples.groupproj`) se ejecutarán correctamente. Si eso aún no sucede, sigue los pasos a continuación.

Para ejecutar el SDK de ZeroBounce con cualquier otro proyecto de Delphi:
- abre el proyecto deseado
- Proyecto > Opciones (o SHIFT+CTRL+F11) > Paquetes > Paquetes en tiempo de ejecución
- habilita "Enlazar con paquetes en tiempo de ejecución"
- [opcional] haz clic en los tres puntos de "Paquetes en tiempo de ejecución", busca `C:\Users\Public\Documents\Embarcadero\Studio\{VERSIÓN INSTALADA}\Bpl`, selecciona "Cualquier archivo (*.*)" en la parte inferior derecha, selecciona el archivo `ZeroBounce.bpl`
- ahora puedes importar las unidades del proyecto

### Free Pascal

Para ejecutar este paquete, debes instalar [Lazarus IDE](https://www.lazarus-ide.org/).

Abre Lazarus IDE, selecciona Archivo > Abrir, busca `./packages/zerobounce.lpk` (relativo a la raíz del repositorio), haz clic en "Abrir paquete".

Para ejecutar las pruebas unitarias, los fragmentos de ejemplo o tu propio proyecto con el paquete Zero Bounce, primero instálalo en Lazarus:
- abre el paquete (descrito anteriormente)
- busca la ventana del paquete (con el título "Package ZeroBounce {versión}")
- Compilar
- Utilizar > Instalar

Después de instalarlo, abrir las pruebas (`./tests/unit_tests.lpi`) o los fragmentos de ejemplo (cualquier archivo `*.lpi` de la carpeta `./examples/fpc/`) debería funcionar correctamente.

Para ejecutar el SDK de ZeroBounce con cualquier otro proyecto de FPC:
- abre el proyecto deseado
- Proyecto > Inspector de proyectos > Agregar > Nueva dependencia
- selecciona "ZeroBounce" de la lista
- ahora debería aparecer en "Paquetes requeridos"
- ahora puedes importar las unidades del proyecto