# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/catalog_tickets.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Definir las columnas de la tabla de base de datos
columnas_db <- c("id_ticket", "tipo_atencion", "prioridad", "tipo_ticket", 
                 "nivel_atencion", "categoria", "subcategoria")

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla datos_colaboradores
dbWriteTable(conn, "catalog_tickets", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")


####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################



# Cargar las librerías necesarias
library(readxl)       # Para leer archivos Excel
library(tidyverse)    # Para manipulación de datos
library(tm)           # Para procesamiento de texto
library(tidytext)     # Para tokenización y análisis de texto
library(syuzhet)      # Para análisis de sentimientos
library(topicmodels)  # Para modelado de tópicos
library(ggplot2)      # Para visualización
library(textstem)     # Para lematización
library(widyr)        # Para n-gramas
library(writexl)      # Para exportar a Excel
library(igraph)       # Para redes de bigramas
library(ggraph)       # Para visualizar redes
library(reticulate)   # Para integrar modelos de Python (BERT)

# Configurar entorno de Python para usar BERT
# 1. Verificar si Conda está instalado
if (!reticulate::conda_binary() == "") {
  # 2. Crear un entorno de Conda si no existe
  if (!"r-tensorflow" %in% reticulate::conda_list()$name) {
    reticulate::conda_create("r-tensorflow")
    reticulate::conda_install("r-tensorflow", "tensorflow")
    reticulate::conda_install("r-tensorflow", "transformers")
  }
  
  # 3. Usar el entorno
  use_condaenv("r-tensorflow")
} else {
  stop("Conda no está instalado. Por favor, instala Anaconda o Miniconda.")
}

# Verificar la configuración de Python
print(py_config())

# 1. Cargar el archivo Excel y renombrar la columna
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)  # Leer sin nombres de columna
colnames(df) <- c("Respuesta_Abierta")             # Asignar el nombre "Respuesta_Abierta"

# Eliminar filas vacías y asignar un identificador de documento
df <- df %>% 
  filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  mutate(doc_id = row_number())

# 2. Limpieza de datos y tokenización
df_limpio <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) %>%
  mutate(word = lemmatize_strings(word, language = "es"))

# 3. Análisis exploratorio: Palabras más comunes
word_counts <- df_limpio %>% count(word, sort = TRUE)
print(head(word_counts, 10))

# Gráfico de palabras más comunes
word_counts %>%
  filter(n > 50) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Palabras más comunes en respuestas abiertas",
       subtitle = "Frecuencia de palabras",
       x = "Palabra", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 4. Análisis de Emociones Específicas
# Cargar el léxico de sentimientos en español
lexicon_es <- get_sentiment_dictionary("nrc", language = "spanish")

# Asignar polaridad (positiva/negativa) a cada palabra
df_polaridad <- df_limpio %>%
  inner_join(lexicon_es, by = c("word" = "word"), relationship = "many-to-many") %>%
  group_by(doc_id) %>%
  summarise(polaridad = sum(value, na.rm = TRUE))

# Unir la polaridad al dataframe original
df <- df %>%
  left_join(df_polaridad, by = "doc_id")

# Visualizar la distribución de polaridad (excluyendo NA)
df %>%
  filter(!is.na(polaridad)) %>%
  ggplot(aes(x = polaridad)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Distribución de Polaridad en las Respuestas",
       x = "Polaridad", y = "Frecuencia") +
  theme_minimal()

# Filtrar respuestas con polaridad negativa para revisión manual
df_negativos <- df %>%
  filter(polaridad < 0) %>%
  select(doc_id, Respuesta_Abierta, polaridad)

write_xlsx(df_negativos, path = "Respuestas_Negativas.xlsx")

# 5. Análisis de Redes Semánticas
# Extraer trigramas
trigramas <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(trigrama, Respuesta_Abierta, token = "ngrams", n = 3) %>%
  separate(trigrama, into = c("palabra1", "palabra2", "palabra3"), sep = " ") %>%
  filter(!is.na(palabra1), !is.na(palabra2), !is.na(palabra3)) %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es"),
         !palabra3 %in% stopwords("es")) %>%
  unite(trigrama, palabra1, palabra2, palabra3, sep = " ") %>%
  count(trigrama, sort = TRUE)

# Visualizar trigramas más comunes
trigramas %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(trigrama, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Trigramas más comunes en respuestas abiertas",
       x = "Trigrama", y = "Frecuencia") +
  theme_minimal()

# Análisis de co-ocurrencia de palabras
co_ocurrencia <- df_limpio %>%
  pairwise_count(word, doc_id, sort = TRUE)

# Filtrar las aristas con mayor peso (n) para simplificar el grafo
co_ocurrencia_filtrado <- co_ocurrencia %>%
  filter(n > 10)  # Ajusta este umbral según sea necesario

# Crear el grafo a partir del dataframe filtrado
co_ocurrencia_graph <- graph_from_data_frame(co_ocurrencia_filtrado, directed = FALSE)

# Verificar el grafo
print(co_ocurrencia_graph)

# Visualizar la red de co-ocurrencia
ggraph(co_ocurrencia_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, color = "gray") +
  geom_node_point(color = "darkred", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Red de Co-Ocurrencia de Palabras",
       subtitle = "Relaciones entre palabras que aparecen juntas")

# 6. Uso de Modelos de Lenguaje Avanzados (BERT)
# Cargar tokenizador y modelo BERT en español
transformers <- import("transformers")
tokenizer <- transformers$BertTokenizer$from_pretrained("dccuchile/bert-base-spanish-wwm-cased")
model <- transformers$TFAutoModel$from_pretrained("dccuchile/bert-base-spanish-wwm-cased")

# Función para obtener embeddings de un texto
get_embeddings <- function(texto) {
  inputs <- tokenizer$encode_plus(texto, return_tensors = "tf", max_length = 128, truncation = TRUE, padding = "max_length")
  outputs <- model(inputs)
  embeddings <- outputs$last_hidden_state[, 1, ]  # Usamos el embedding del token [CLS]
  return(embeddings)
}

# Aplicar la función a todas las respuestas
embeddings <- t(sapply(df$Respuesta_Abierta, get_embeddings))

# Realizar clustering en los embeddings
kmeans_result <- kmeans(embeddings, centers = 5)  # Ajusta el número de clusters según necesidad
df$cluster <- kmeans_result$cluster

# Visualizar la distribución de clusters
ggplot(df, aes(x = as.factor(cluster), fill = as.factor(cluster))) +
  geom_bar() +
  labs(title = "Distribución de Clusters", x = "Cluster", y = "Frecuencia") +
  theme_minimal()

# Mostrar palabras clave por cluster
palabras_por_cluster <- df_limpio %>%
  left_join(df %>% select(doc_id, cluster), by = "doc_id") %>%
  group_by(cluster, word) %>%
  summarise(n = n()) %>%
  top_n(10, n)

print(palabras_por_cluster)


####################################################################################
####################################################################################
####################################################################################
####################################################################################


Respuestas abiertas.xlsx

¿Qué necesidades de conocimiento o información acerca del programa SMART CORE, específicamente sobre la iniciativa “Disminuir el número de cancelaciones en solicitudes aprobadas por la Metodología” requieres?
  SE EVITA CON UNA BUENA ASESORIA AL SOCIO
todo bien
CONSIDERO QUE CON LA INFORMACIÓN PROPORCIONADA ES SUFICIENTE
IMPORTANTE QUE SE ENVIE RECORDATORIO MEDIANTE PLATAFORMAS ACTUALES A LOS SOCIOS
Considero que se nos ha proporcionado toda la informacion necesaria para conocer y saber el porque de las cancelaciones de las solicitudes, situaciones que debemos trabajar para poder finalizar con la entrega de los creditos
NINGUNA
.
.
22
NADA
SIN COMENTARIOS
GRACIAS
ninguno
.
.
Excelente contenido.
TODO MUY ENTENDIBLE
muy bien
BUEN CURSO
Todo bien.
LA INFORMACION ESTA MUY COMPLETA
OK
...
sin comnetarios
o
Buen tema
HASTA EL MOMENTO TODO ESTA EN ORDEN
.
gracias
todo bien
Tdo claro
OK
Excelente iniciativa de mejora en beneficio del servicio a los socios
gracias
Todo bien
NINGUNA
ninguna
ninguna
ninguno
Por el momento queda claro el proceso a seguir con la iniciativa de disminuir el número de cancelaciones en solicitudes aprobadas por la metodologúa, será necesario trabajar en ello para poder ver que mas se debe considerar para mejorar
todo bien
bien
EXCELENTE INICIATIVA PARA MEJORAR LA EXPERIENCIA DEL SOCIO
por el momento siento que cuento con los conocimientos necesarios
todo bien
EL CURSO ES ENTENDIBLE
NINGUNA
todo bien
*
  GRACIAS
con la marcha por esta nueva implemnetacion veremos las alternativas o mejoras del sistema bajo esta modalidad y el beneficio de la sucursal con el incremento de oportunidades de credito.
NINGUNO
Ninguno
muy bien
curso
De momento todo en orden
ninguna
execlente  contenido
TODO BIEN
excelente Información
la capacitación e información ha sido esencial y buena para mejorar el servicio, el tema toca temas importantes como " el socio como centro de nuestro servicio" para conocer la experiencia de servicio del socio y que esta mejore también se deberá cuidar la parte laboral o talento humano, mediante estos activos de capacitación manteniendo un personal informado, capacitado, con plantillas completas y sobre todo con satisfacción de pertenecer a CPM, es por ello que es importante el cliente interno o la fuerza laboral esperadando que se mantenga así este año mediante incrementos salariales competitivos al mercado y acordes las nuevas metas y NO con las diferencias de los últimos años entre personal de confianza y sindicalizados.
GRACIAS
....
.
No identifico alguna por el momento
estar reforzando o ampliando las capacitaciones sobre el tema de servicio y dar un seguimiento
QUE SE CONOZCA MAS A DETALLE LO QUE ES EL PROGRAMA Y SUS DIMENSIONAMIENTOS
...
buena imformacion
todo bien
TODO BIEN
todo bien
ninguna
.
ninguno
sas
gracias
.
ninguna
muy bien
QUE EL SISTEMA DE BURO DE CREDITO DE MAS MARGEN EN EL HISTORIAL DEL SOCIO YA QUE EN OCACIONES LA DEUDA YA NI SIQUIERA EXISTE Y EL SISTEMA DE CPM LOS AFECTA DEMASIADO EN ESE SENTIDO
TODO BIEN. SOLO EN LO PERSONAL CONSIDERO QUE PARA ESTE TIPO DE TEMAS QUE SON CAMBIOS IMPORTANTES PARA LA INSTITUCION DEBERIAN DAR UN TIEMPO FUERA DE LA OPERACION CON EL FIN DE QUE LA INFORMACION SE RECIBA DE MEJOR FORMA Y DE IGUAL MANERA PARA TODOS. GRACIAS.
el curso es claro, aun esta pendiente otro curso al cual estoy convocada el dia de mañana
..
niniguna
Gracias
gracias
.
por el momento con lo muestra la metodologia es lo nesesario
ok
acceso a cancelaciones, vistas de oportunidades, status de canceladas.
gracias
sin comentarios
NINGUNA
por mi puesto no realizo ese proceso pero estaria checando con los compañeros cuando surga alguna duda o inquietudes
listo
Muy buena iniciativa, con esta información y sobre todo llevando a la practica podemos ofrecer experiencias de servicios que sean positivas y que se vea reflejado en los indicadores, pero sobre todo en la recomendación positiva del socio
siempre tener presente el proceso y la normativa con los compañeros
ninguno
estar capacitados en la normativa , procesos , estudiar para asi brindarle al socio una bonita experiencia de que su tramite es facil y rapido . que vea que cpm es la mejor opcion cuando tenga una necesidad.
a
sin comentarios
Retroalimentacion continua para evitar equivocaciones y reprocesos.
Informacion sobre reporteria mensual para dar seguimiento al comportamiento y resultados de esta estrategia
es muy bueno que antes de iniciar cualquier curso nos den una introduccion, y esto nos ayuda a motivarnos para poder trasmitirlo a nuestros compañeros y muy dinamico y comprensible
.
TODO BIEN
ok
ok
hay que asesorar al socio de forma correcta para dar el mejor servicio ,  y hacer que la primero opcion para ellos sea caja popular mexican
BN
.
X
Técnicas para crear necesidades en los socios y continuen con los procesos de créditos
=)
POR EL MOMENTO NINGUNA
NINGUNA POR EL MOMENTO
TODO BIEN
las guias lo muestran
ninguno,
Ok
.
.
.
ME PARECE CLARO
SIN COMENTARIOS
Exisrtirfa tambien un canal o candados adicionales en la app par los que realizan tramite de credtio en app ??
  .
Siento que se trata también de la actitud que tengamos como colaboradores, porque en ocasiones tenemos todos los elementos para apoyar al socio y no lo hacemos, la falta de seguimiento en las sucursales, pues hay colaboradores que cancelan las solicitudes por cancelar y no dan el seguimiento necesario para la necesidad del socio.
entendible la informacion
Excelente curso
que se nos de capacitacion presencial sobre los cambios en el sistema para una mejor comprension
que porcentaje determina cada valoracion para la viabilidad de la solicitud
ok
SIN COMENTARIOS
muy buen idea, solo que la mayoria de las solicitudes si no casi todas se cancelas por que el socio cuenta con un mal historial creduticio externo o no cuenta con la documentacion para realizar el tramite, ya que hay muchos trabajos infomarmales que no expiden cartas laborales.
en mi plaza diario preguntan por las solicitudes en formalización, se le llama al socio y ya no contesta, por esto se cancelan
que puedan manda los documenos digitales tambien a sucursal ya que a veces no los traen impresos y por ir a imprimirlos ya no vuelven
solo que estemos actualizados
MATERIAL SERA DE GRAN UTILIDAD
ninguna
.
TODO BIEN
todo muy bien
GRACIAS
ninguno
RECIBI CURSO
excelente curso ayuda a conocernos mas como personas y a espresarnos mejor em
NINGUNO
Ninguna
modificar normativa, ser mas empáticos con los socios ya que el reunir muchos documentos hace que el socio pierda el interés por seguir con el tramite.
los cambios siempre requieren de un impulso que nosotros debemos de dar para transformar la experiencia del servicio al socio
.
,
TODO MUY BIEN
ok
Todo en orden almomento
bien
NINGUNA
ES RELATIVA LA ESTADISTICA MENCIONADA EN EL PRESENTE TRABAJO, NO ES ABSOLUTO EL QUE NO HAYAN TENIDO UN CREDITO LOS SOCIOS QUE EN SU MOMENTO CANCELARON, SERIA MAS COMPLETA SI CONSIDERAMOS  A LOS SOCIOS QUE SE LES REALIZO CANCELACION Y POSTERIORMENTE SE LES ENTREGO UN CREDITO A TRAVES DE OTRA OPORTUNIDAD... SALUDOS.
buen curso
verificar la necesidad del socio para asi ofrecerle el producto adecuado plazo y monto pedirle la documentacion que ocupa de acuerdo a su actividad y la atencion que brindamos
nada que manifestar
h
MUY "BONITA" LA INICIATIVA  Y CON BUENAS INTENCIONES EL PROYECTO; SIN EMBARGO, SOLO ESTA LA VERSIÓN DEL SOCIO QUIEN CABE ACLARAR NO TIENE LA VERDAD ABSOLUTA.
NINGUNO
SOLICITUDES CON RESULTADO VIABLE EN SUCURSAL Y QUE VIAJAN  A CAPYC Y REGRESAN COMO DECLINADAS AUN SIENDO CON RESULTADO VIABLE CUANTOS DIAS SE LE ESPERARA AL SOCIO PARA QUE ENTREGE SUS DOCUMENTOS
Es importante conocer la necesidad del socio y que se sienta acompañado en el proceso hacer que su customer journey sea de calidad y si es necesario darle a conocer la información de nuestros servicios móvil considerando que no podrá regresar físicamente a realizar su tramite.
GRACIAS
ninguno
NINGUNA HASTA EL MOMENTO ESTA MUY COMPLETA
.
gracias
Gracias.
Que se encuentre ya en función en el aplicativo crm para ponerlo en practica.
NINGUNA
.
.
Que nos brinden avance de los resultados sobre esta iniciativa
todo bien hasta el momento
gracias
:)
La iniciativa es acertada, esto facilitará la comunicación sucursal-socio
ninguna, todo esta bien
.
Todo bien!
  TODO BIEN
OK
Ninguna
TODO ME PARECE MUY BIEN SOLO DE NUESTRA PARTE ESTAR ACTUALIZANDO TODO DE MANERA CORRECTA Y ESTAR REVISANDO EL MOS CONSTANTEMENTE PARA ESTAR BEIN INFORMADOS Y ASI PODER DAR UNA MUY BUENAS ASESORIA A NUESTROS SOCIOS.
es muy buena la informacion
SIN COMENTARIOS
creo que todo bien
ninguna
APOYAR DE MANERA ADECAUDA
.
.
.
PONER EN PRACTICA LO APRENDIDO
Gracias
Muchas gracia por su apoyo y compartirnos la información para bien de la Institución
bien
Excelente informacion que nos compartieron ya que nos ayudo a fortalecer y reafirmar la importancia de ofrecer una excelente atencion a todos nuestros socios siendo empaticos en sus necesidades no olvidando que ellos son el principio y fin de nuestra institucion y que los debemos de cuidar para mantenerlos cautivos con nuestros productos y servicios. GRACIAS A LOS EXPONENTES POR ESTA INFORMACION.
LA FECHA A PARTIR DE CUANDO SE IMPLEMENTARA
excelente capacitacion
EXCELENTE
ES UNA BUENA ESTRATEGIA PARA OFRECER BUENAS EXPERIENCIAS A NUESTROS SOCIOS
.
2
.
LA INFORMACION EN LAS PRESENTACIONES ES BUENA, COMPLETA CON LA PRACTICA Y EXPERIENCIA OBTENIDA SE LOGRARA LA DISMINUCION.
excelente
ESTAR INFORMADO ANTE CUALQUIER CAMBIO ASI COMO LOS PROCESOS A REALIZAR
yo considero que  las capacitaciones que nos brindan están muy bien  y son las adecuadas para que nosotros las llevemos a cabo
POR EL MOMENTO NINGUNA ADICIONAL. GRACIAS.
INFORMACION MUY IMPORTANTE PARA MEJORAR LA ATENCION Y SERVIICO AL SOCIO
LA INFORMACION ES CLARA Y COMPLETA
ninguna
ni guna
por el momento ninguna
Gracias
que nos sigan reforzando con cursos referente a estos cambios porque dia a dia aparecen cambios con nuestros sistemas.
.
muy buen material
ninguna
LA PREGUNTA 9 NO LE ENTENDI
NINGUNO. GRACIAS
en este nuevo proyecto considerar las solicitudes no viables por buro de crédito denegado para este tipo de solicitudes no son viables
gracias
Considero que con la información que se nos proporciono tenemos lo necesario para dar seguimiento oportuno a las solicitudes para que la cancelación de las mismas disminuya.
.
gracias
TODO BIEN
POR LO PRONTO NINGUNA
LA INFO PARA ESTUDIO
...
.
Quedamos en espera de que nos bajen la informacion vía correo institucional
MUY BUEN CURSO
ninguno
Continuar con las retroalimentaciones son buenas para nosotro como colaboradores
ninguna por el momento
exelente infrmacion
todos los cambios y nuevos precesos sobre todo en el ambito tecnologico son siempre para bien y mejorar la calidad del servicio debemos tener apretura parala implementacion de todo lo concerniente a competir de manera digna con otras instituciones y asi mejorar las espectativas de los socios
ninguno
Tenemos las herramientas para llevar acabo los nuevos retos y procesos
Escuchar al socio
todo bien
ninguna
NINGUNA
.
continuar con la retroalimentacion de cada programa
interesante
NINGUNO
*
  todo bien
LA INFORMACION QUE SE ME PROPORCIONO ES BUENA, LA ENTENDI PERO LO MAS IMPORTNTE ES AL MOMENTO DE PONERLA EN PRACTICA QKUE YO MISMA PUEDA COMPROBR QUE EN EFECTO ME ES UTIL EN MIS ACTIVIDADES DE TODOS LOS DIAS Y QUE PUEDA OBNTENER EL RESULTJADO QUE SE ESPERA
buen tema
sin
.
ninguna
NA
buen contenido dinamico y sencillo
NINGUNA
.
.
NINGUNA
mas informacion detallada del modelo
.
BIEN!
  TODO BIEN
informacion clara
algunas precisiones como quienes son los unicos facultados para cancelar las solicitudes, cuando se pueden cancelar las solicitudes con mas amplitud
CONSIDERO QUE LO QUE REQUERIMOS DE CONOCIMIENTO E INFORMACION SE NOS ESTA PROPORCIONANDO CONFORME AVANZAMOS EN EL PROCESO POR LO QUE NO CONSIDERO TENER NECESIDADES AL MOMENTO, SI NO ESTAR ATENTOS Y CON EL COMPROMISO EN EL PROCESO.
ninguna
ok
ninguna
ok
BUEN CURSO
.
.
excelente
muy buena informacion
ninguno
.
MUY BUENA ACTUALIZACIÓN
gracias
todo en orden
RUTAS DONDE ENCONTRAR LAS DIFERENTES OPCIONES DE CONUSLTA DE SOLICITUDES
gracias, tener a la mano guías
exelente info
sin comentarios
:9
CONSIDERO QUE ESTA NUEVA ACCION ESTARA  REFORZANDO LA PARTE DE DARLE SEGUIMIENTO A  ASESORIAS Y ASI MISMO CREEARLES LA NECESIDAD Y   ACUDAN A SUCURSAL, ASI MISMO LOS SOCIOS SENTIRAN UNA ATENCION MAS PERSONALIZADA AL IMPLEMENTAR ESTO
QUE EL SOCIO COMPRENDA LA DOCUMENTACION QUE SE LE SOLICITA DESDE UN INICIO PARA CONTINUAR CON SU TRAMITE Y EN SU CASO QUE CPM SOLICITE MENOS DOCUEMTACION PARA EL ANALISIS DE LOS CREDITOS QUE EL SOCIO HA TENIDO EN CPM EN SU HISTORIAL CREDITICIO
NO HAY
.
.
excelente curso
NECESITAMOS QUE CUALQUIER CAMBIO SE NOS INFORME SOBRE ESTE TEMA TAN IMPORTANTE. NECESITAMOS GUIAS Y POLITICAS DONDE PODAMOS REVISARLOS EN CUALQUIER DUDA QUE TENGAMOS.
SIN COMENTARIO
me parece que fue muy completa la informacion del curso
ok
.
.
estar en costante actualizacion
.
LA INSTITUCION NOS PROVEE LAS HERRAMIENTAS NECESARIAS
Excelente
retroalimentacion en este nuevo proceso de manera mensual sabiendo en cada mes como se esta dando el servicio al socio y que mejoras en cuestion de atencio se les puede brindar para tener mayor confianza.
CREO QUE TODO ESTA BIEN SOLO IRNOS ACTUALIZANDO EN CUANTO LOS CAMBIOS QUE SE VAYAN DESARROLLANDO
sin comentarios
EN CUESTIONES DE REQUISITOS PORQUE EL AREA DE CREDITO A VECES NOS SOLICITA DEMASIADOS.
sin comentarios
BUENA IMPLEMENTACION
todo muy claro
FUE MUR FACIL DE ENTENDER GRACIAS
buen curso
Ninguno
la información brindada en el seminario así como en el material me ayudan a conocer mas acerca de como disminuir las cancelaciones además de como poder explicarlo a algún compañero que requiera apoyo
muy bien
conciencia
Un seguimiento directo en sucursales.
TODO BIEN
BIEN
LAS ESPECIFICACIONES QUEDARON CLARAS.
Hasta el momento vamos avanzando sin complicaciones
TENR MAS CONTACTO CON EL SOCIO POR MEDIO DE OTROS MEDIOS PARA QUE PUEDA COCLUIR CON SU TRATIME
me quedo claro con toda la informacion proporcionada
INFORMACIÓN CLARA
d
ninguna
POR EL MOMENTO NINGUNA
Conocer la voz del socio será de suma importancia para el éxito futuro de cualquier programa.
ok
.
Generacion de vistas
buena presentacion
seguir mejorando para brindar el mejor servicio a nuestros socio y de ese modo nuestra cooperativa siga creciendo
muy buena presentacion y motivacion para ser mejor persona preparada para cualquier cambio para el bien de la institucion y los socios
.
ok
1
Por el momento todo quedo claro
de acuerdo
1
BUENO
CONSIDERO QUE ES UNA HERRAMIENTA QUE PROPORCIONA OPORTUNIDADES DE CRECIMIENTO EN COLOCACION Y PODER OFRECER UNA MEJOR CALIDAD EN EL SERVICIO
muy bien
EXCELENTE CURSO
por el momento ninguna, y que los ocmpañeros realisen un buen servicio al socio, ya que vemos que cancela por los tiempo de espera en sala y los requisitos que solicitan
por el momento todo esta en orden
sin comentarios
.
TODO AL 100
ninguno
NINGUNA
toda la informacion es muy completa por lo que nos ayudara a mejorar nuestro servicio y poder lograr cumplir con nuestras metas.
OK
SE DEBERIA ANALIZAR CUANDO EN SISTEMA ES VIABLE PERO POR HISTORIAL INTERNO SE CANCELAN O AGREGAR ESA OPCION SE CANCELA POR MAL HISTORIAL INTERNO
un reforzamiento practico
na
SIN COMENTARIO
NINGUNA
.
NINGUNA
POR EL MOMENTO SE CUENTA CON LA INFORMACIÓN NECESARIA PARA COMENZAR CON LOS NUEVOS CAMBIOS
.
gracias
sin comentario
bien
.
gracias
ecxelente
.
me parece importante la informacion para el seguimiento de solicitudes
en general me pareció muy completo el curso
CONOCER EL TIPO DE SOCIO Y ACTIVIDAD YA QUE AL MOMENTO QUE ACUDE EL SOCIO A SUCURSAL A CHECAR EL CREDITO PREAUTORIZADO A ALGUNOS SOCIOS CON BUEN HISTORIAL Y CAPACIDAD ECONOMICA NO LES OFERTA NADA.
ninguno
sin comentarios
todo bien
gracias
5
SIN COMENTARIOS DE MOMENTO
SE REQUIERE MAYOR SEGUIMIENTO CUANDO SE DE EL CAMBIO DE LA CANCELACION DE OPORTUNIDADES
OK
..
EXCELENTE
a
ES IMPRTAMNTE CONOCER DICHA INFORMACION PARA CON ELLO EVITAR Y DAR EL MAYOR A SEGUIMIENTO A SOLICITUDES EVITANDO LA CANCELACION DE ELLAS
gracias
.
POR EL MOMENTO NO TENGO NINGUNA, CON LA INFORMACION PROPORCIONADA EN EL CURSO HA SIDO SUFICIENTE.
ok
ME QUEDO CLARA LA INFORMANCIÓN CON LA CAPACITACIÓN DEL CURSO, GRACIAS
detalles de funcionalidades del programa
Ha quedado clara la información
.
BUENA INICIATIVA
sin comentarios
caidad de asesoria,entorno de sucursal,tiempo,personalizacion,alternativas
FUE MUY BUENA LA INFORMACION RECIBIDA
.
GRACIAS POR LAS ACTUALIZACIONES Y CAMBIOS SON PARA MEJORA DEL SERVICIO A NUESTROS SOCIOS Y MAYOR ALCANCE DE METAS
v
SERIA BUENA IDEA QUE SE DIERA UNA CAPACITACION DE SERVICIO PERO DE MANERA PRESENCIAL
MUY COMPLETO
ninguna
.
Ninguno
bien
muy completo
LEER BIEN LA INFORMACION QUE NOS PROPORCIONAN
sin comentarios
.
pues basicamente es una buena iniciativa para que se evalue el por que se cancelan las solicitudes viables , al ser varias las causas ,
DE MOMENTO CON EL MATERIAL DESCARGABLE ES SUFICIENTE
TODO BIEN
NA
considero que contamos con el conocimiento requerido para ofrecer la asesoría de calidad y erradicar las situaciones mencionadas durante este curso
ninguna
ninguna
practicas en sistema
necesidad de un buen internet yaq ue a cada rato el sistema se va
estuvo muy padre este curso rapido, entedible y resumido los temas
......
very good
en planes de pago que al cotizar varios no se actualizan y hay que cancelar la oportunidad porque si no se marca con error
excelente capacitacion
ESTAR CON ACTITUD PARA LOS CAMBIOS
ninguna
gracias
.
Todo bien
NINGUNA SOLO NECESITO ESTAR REFORZANDO LA LECTURA DE LA INFORMACION QUE NOS BRINDARON
SIN COMENTARIOS
todo bien
nada
Quedó todo claro
EXCELENTE MATERIAL
GRACIAS
ninguna
.
NINGUNA, EL CURSO ME AYUDO PARA CONOCER Y PODER BRINDAR UN MEJOR SERVICIO A NUESTROS SOCIOS
Todo muy bien
UN SI AL CAMBIO
QUE LAS SOLICITUDES QUE EL SISTEMA DETECTE COMO NO VIABLES AL CORRER EL SCORING SE CANCELEN AUTOMATICAMENTE.
nos ayuda a tener mas conocimientos para mejor servicio
buena información y buen compromiso por parte de cooperativa hacia con el socio.
..
.
Excelente.
CON EL MATERIAL Y LA INFORMACION ES SUFUCIENTE
.
muy importante la informacion
NINGUNA TODO BIEN EXPLICADO EN EL CURSO
.
QUE DE PREFERENCIA SEA EL MISMO ASESOR QUIEN FINALICE EL TRAMITE ,  YA QUE EL SOCIO CUENTA CON UNA BUENA EXPERIENCIA
GRACIAS
seguir  estudiando
BIEN
ninguno
GRACIAS
:)
ninguna es claro el objetivo y esperamos buenos resultados
a
ninguno, todo bien al momento
gracias
.
gracias
excelente
sc
sin comentarios
TODO BIEN
COMO PROPUESTA DEBERIA CONSIDERARSE REALIZAR QUE ALGUNOS CURSOS SE LLEVEN A CABO DE MANERA PRESENCIAL,
------------------
  BUEN CONTENIDO EN LA INFORMACION
El impacto que se tenia con dichas cancelaciones de solucitudes
.
PUES CONSIDERO CLARO QUE MI PRINCIPAL FUNCION ES DETECTAR LA NECESIDAD DEL SOCIO PARA PROPORCIONARLE UN BUEN PRODUCTO DE CREDITO O SERVICIO, ACOMPAÑADO DE EMPATIA Y CONFIABILIDAD DE LO QUE EL REQUIERE ASI COMO CONVENCERLO DE QUE SOMOS SU MEJOR OPCION.
ninguna
gracias
Excelnete
ok
SC
ok
na
ok
la iniciativa es buena.
EN MI OPINION LA CANCELACION DE SOLICITUDES EN LA MAYORIA DE LOS CASOS ES O PORQUE TIENEN UN BURÓ CON MAL HISTORIAL AUNQUE LA METODOLOGIA LO DETERMINE COMO NIVEL DE RIESGO MEDIO ALTO, PORQUE QUIEREN SABER COMO ESTAN EN BURO DE CREDITO O POR INDECISION DEL MISMO, QUE ESTAN ENTRE QUE SI QUIEREN EL COMPROMISO O NO,  POR LO REGULAR ES POR LO QUE SE CANCELAN.
sn
.
Considero que con la información proporcionada es suficiente por le momento, sin embargo en la implementación pueden surgir dudas al respecto, recomiendo el seguimiento para que estos cambios sean exitosos y se logre el objetivo que buscamos que es mejorar la calidad de los socios, en este caso disminuyendo la cancelación de solicitudes de crédito, Felicidades a Isa, Rocio, Brenda y Darinel muy bien explicado los temas, sobre todo en lo personal me motivaron.
esta super bien realizar mejoras en nuestros sistemas y servicios para mejorar la experiencia de nuestros socios  y podamos dar agilidad a sus tramites y servicios
..
es muy importante conocer las causas por las cuales los socios no continúan con su tramite y con este programa nos permite conocer las estadísticas para poder darle una buena alternativa al socio para mejorar su experiencia en cpm
SIN COMENTARIOS
ok
.
EXCELENTE INFORMACION
na
NINGUNA, CTO CON LA INFORMACION NECESARIA
NINGUNO
ok
Tengo el conocimiento y me siento capaz de dar un buen servicio asi como buena asesoria para no cancelar solicitudes viables
EN EL MOMENTO NO SE ME OCURRE NINGUNA SOY DE LAS PERSONAS QUE CREE QUE LA PRACTICA A SABER EN QUE PARTE NECESITO MAS APOYO PARA APREDER DE LOS NUEVOS PROCESOS Y OCURRAN MENOS CANCELACIONES,,,
AGRADECER LA INFORMACION
ok
MAYOR CAPACITACION
SOLO PUEDO DECIR QUE SERA DE UN GRAN APOYO PARA QUE NUESTROS SOCIOS TENGAN PRESENTA LOS REQUISITOS Y SE PUEDA CUMPLIR CON EL MAYOR NUMERO DE SOLICITUDES APROBADAS.
.
curso interesante
CONSIDERO QUE EL APOYO QUE NOS BRINDARAN CON LOS MENSAJES DE RECORDATORIO AL SOCIO SON UNA GRAN HERRAMIENTA QUE NOS AYUDARA A QUE SEAN MENOS LAS SOLICITUDES QUE ESTEN PENDIENTES DE FINALIZAR, GRACIAS.
OK
SIN COMENTARIOS
.
NINGUNO
HABILITAR MOS
ok
siempre ayuda contar con material de apoyo
0
ninguna
buen curso
0
exelente informacion
MUY BIEN
BUEN CURSO
ok
gracias
NINGUNO
esta muy completo el programa
"El tratar de tener algo un poco mas simplificado sin necesidad de quitar los requerimientos, que se pudiera reducir tanto papeleo el cual es el que le incomoda
a los socios el tener que conseguir como el de firmar muchos papeles simplificandolo cpn firmas digitales o su nip seria un ejemplo."
sin dudas
NINGUNO
todo muy bien
sc
excelente
.
ok
Soy capaz de adaptarme para poder aprender y despues usar el conocimiento y la eficacia del uso de nuevos programas en pro de la CIA, y agregando el valor en cuando a un servicio de calidad, sin embargo me gustaria ganar mucho mas por los aportes que hago, mis numeros hablan por si solos.
el tema quedo muy claro,ya solo es cuestion de conocer sobre la marcha las necesidades que lleguen a surgir
CONSIDERO QUE ESTOS CURSOS SON BUENOS AUN ASI CADA VEZ SE VE, MENOS EL INTERES EN EL CONTACTO DIRECTO DONDE SE DABAN LAS CAPACITACIONES PRESENCIALES O AL MENOS UN POCO MAS DE SUSTENTO PERSONALIZADO.
EXCELENTE REFORZAMIENTO
SIEMPRE ES MUY IMPORTANTE ACEPTAR EL CAMBIO, Y LAS MEJORAS  A UN NUEVO SISTEMA PARA NO QUEDARNOS OBSOLETOS Y DE ESTA FORMA BRINDAR UNA MEJOR EXPERIENCIA AL SOCIO.
SIN COMENTARIOS
EXCELENTE EL CURSO
.
gracias.
todo bien
gracias
QUE SE NOS AVISE DE MANERA OPORTUMA DE CUALQUIER CAMBIO A IMPLEMENTAR
.
CURSOS PRESENCIALES
CON EL MATERIAL DESCARGABLE SE ENTIENDE LA INFORMACION YA LAS DUDAS SALDRAN DURANTE LA APLICACION
INFORMACION MUY UTIL
DE MOMENTO NO TENGO NINGUN REQUERIMIENTO DE INFORMACION
Ninguna todo me quedo muy claro gracias
La informcion es excelente, solo que como ejecutivo de credito pcr no me pemite ver  mis solicitudes pendientes, tengo que revisar una a una y a veces no es psoble por la contidad de socio que atendemos, seria bueno que habilitaran este parte tambien para los que somos pcr.
guías y criterios para apoyar a nuestros socios ya que cada socio es un caso diferente.
n
buena info
..
considero que la informacion quedo clara durante el curso, muchas gracias!
  CON LA INFORMACION PROPORCIONADA QUEDO CLARO EL TEMA, MUCHAS GRACIAS!
  .
pudiera ser que el socio deposite la garantia en cuenta mexicana hasta que su credito este aprobado, asi pudieran tener la certeza de que se les entregara el credito
excelente inf
Todo bien gracias.
*
  sin comentarios
muy buenas tips y tambien la informacion proporcionada
.
.
"POR EJEMPLO CUANDO TRAEN BURO DENEGADO POR CUENTAS ABIERTAS CON MOP 96 O 97 POR CANTIDADES PEQUEÑAS EL SOCIO PODRA CONTAR CON LA AUTORIZACION DEL CREDITO? 
QUE PASA SI UN SOCIO TRAE SU PAPELERIA UN DIA ANTES DE SU VENCIMIENTO Y LE APLICA VERIFICACION EL SISTEMA NO CANCELARA LA SOLICITUD?"
que los parametros de calificacion sean mas convicentes y asi mismo nos permita realizar la aprobacion de los creditos y asu vez la cancelacion
GRACIAS
INFORMACIÓN MAS CLARA
QUE SE FIRMEN LA SOLICITUD AL MOMENTO Y QUE SE DE OPCION AL SOCIO DE ENVIAR SU DOCUMENTACION VIA CORREO ELECTRONICO O A UN NUMERO DE WATSAP PARA QUE SE FORMALIZEN ESE MISMO DIA O POSTERIOR, AUNQUE SE VERIAN AFECTADOS LOS TIEMPOR PARA SU RESOLUCION.
TODO BIEN
sin
buena informacion
muy clara la informacion
muy buenas presentacion
CURSO PRACTICO
ME HA PASADO QUE LAS CANCELACIONES DE SOLICITUDES DE CREDITO, SON ´POR LAS TASAS ALTAS YA QUE ALGUNAS HABLANDO DEL NIVEL DE REISGO MEDIO EN ADELANTE NO SON COMPETITIVAS CON LAS QUE OFRECEN EN LOS BANCOS ADEMAS DE QUE EN LOS BANCOS NO LES PIDEN PAPELERIA SE LOS DAN EN EL MOMENYO, Y AQUI ES TRAER PAPELERIA Y LUEGO REGRESAR POR LA ENTREGA, Y OTRO PUNTO ADICIONAL ES EL MONTO DE LAS POLIZAS DE SEGURO DE AUTO PARA USO PRDUCTIVO, ESTAN EXEDIENDO LOS MONTOS, CADA VEZ ESTAN MAS CARAS INCLUSO REVASANDO LOS 50,000 ANUALES, COSA QUE ASUSTA A SOCIO Y DESISTEN DE HACER LA SOLICITUD DE UN CREDITO AUTOMOTRIZ. ESPERO MEJOREN ESO TAMBIEN.
exelente
ME SIENTO PREPARADA DENTRO DE MI AREA DE TRABAJO PARA HACER MI TRABAJO LO MEJOR POSIBLE PARA CUMPLIR CON LAS METAS QUE NOS PONEN
NA
.
bien
"¿FORMA DE CANCELACION ANTES DE LA FECHA DE 30 DIAS CUANDO ES VIABLE?
¿CUAL ES LA FECHA CON MEJOR OPCION QUE SE INDICARÁ AL SOCIO PARA PRESENTAR DOCUMENTACION O SE INDICARÁ FECHA MÁXIMA DE PERIODO PARA ENTREGA DE DOCUMENTOS?"
Las guías, las retroalimentaciones cubren el conocimiento que debo tener para brindar un mejor servicio, por otra parte depende de cada colaborador logrargo hacer el match perfecto, gracias.
DEACUERDO
XD
SIN COMENTARIOS
excelente presentacion
..
EXCELENTE APRENDIZAJE
MUY IMPORTANTE
Ninguna
.
buen curso
.
facilitar la documentacion de socio como comprobantes de domicilio con variantes en nombre o colononias
sc
IMPLEMENTAR TODOS LOS ELEMENTOS DE APOYO QUE TENEMOS AL ALCANCE
.
QUE NOS MANTENGAN INFORMADOS CON CURSOS O REUNIONES PARA DISIPAR DUDAS .
................................................................................................................................................................................................................................................................................................................................................................................................
NINGUNA
buen contenido
☺
BUEN CURSO
.
:)
.
wooo excelente informacion
buena capacitacion
Por el momento no
muy bien =)
NO
BUENOS MUY BUENOS CAMBIOS
NIGUNA
solo la actualizacion de las mejoras constantemente
todo bien
muy completo
n/a
.
informacion clara
MUY BIEN
solo llevarlo a la practica
es muy importante el tema con la intención de disminuir el monto, y procurar la colocacion buena en creditos
gracias
excelente informacion
ninguna
el manejo en el sistema de como podremos interactuar y visualizar las solicitudes en el programa
bien
mejoras y eficiencias en la colocacion
excelente
.
El cambio organizacional como bien se menciona en el curso es una tranformación interna, tomando en cuenta factores internos y externos. Con esta transformación buscamos mejorar los productos y servicios que otorgamos a nuestros socios para cumplir con nuestra propuesta de valor. Tenemos que ser más empáticos con el socio al momento de una asesoría con cualquiera de nuestros productos y servicios, tener el poder de convencimiento de que caja popular mexicana es la mejor opción financiera para cubrir sus necesidades. De esta manera el socio regresará para darle seguimientos a sus procesos en cpm
ok
ninguna
Ningun curso implica el conocimiento en su totalidad, ¨LA PRACTICA HACE AL MAESTRO ¨
ninguna
EXCELENTE CURSO
SIN COMENTARIOS
GRACIAS
DE MOMENTO NINGUNO, TODO ESTA BIEN.
todo ok
MUY BUEN MATERIAL
la informacion fue la necesaria para realizar el curso
.
ok
...
"De momento parece estar todo claro. Quizás en la marcha de los diferentes tramites, pueda surgir alguna duda.
Agradezco el apoyo y capacitación."
gracias
es bueno que nos den este tipo de cursos para estar actualizador y asi poder ayudar a los socios en todas sus dudas
NA
ninguna
na
estoy de acuerdo con el cambio y estaremos felices de la transformacion
Todo bien.
.
feliciddaes
sin comentarios
informacion suficiente
EL PROCESO ES MAS VIABLE PARA NUESTROS SOCIOS Y ACAPARAR MAS GENTE QUE ESTE INTERESADA EN LOS CREDITOS DE CPM YA SIN TANTO PAPELEO
gracias.
posiblemente una guía rápida de crm  para ubicar en que fase o apartado estarán los nuevos cambios que permitiran emitir mensajes, correo de requisitos... gracias
ES SUFICIENTE LA INFORMACION PROPORCIONADA, GRACIAS
...
El proceso es muy similar al que ya se manejaba con CRM anteriormente, por lo que no existen dudas.
bien
TODO BIEN
GRACIAS POR LA CAPACITACION
s/c
curso muy divertido
SE ME HACE DE SUMA IMPORTANCIA
la información es clara, se entiende que es parte de la estrategia, solo apegarnos a normativa
Buen curso
GRACIAS
h
excelente
INFORMACION DE AYUDA PARA DAR MEJOR SERVICIO
todo muy bien, el tema bastante claro.
BUEN CURSO
EXCELENTE CURSO
ok
bien
me queda muy claro
.
TODO CLARO
Ninguno, esta bien
ha sido buena la informacion se esta trabajando para acercar los productos a nuestros socios, en sucursal se prioriza el servicio brindado.
necesitamos mas cursos sobre servicio al socio (presenciales)
POR EL MOMENTO LA INFORMACION ES CLARA
SIN COMENTARIOS
SOLO QUE NOS APOYEC CON EL MATERIAL DE COMO VA A SER AL FINAL DEL PROCESO LA ACTIVACION DE CASILLAS PARA RECORDAR TODO Y NO COMETER ERRORES
buen curso
Por el momento nada :)
.
GRACIAS
AUN NO RECIBO LA CAPACITACION
.
.
SEGUIR CON ESTE TIPO DE CURSOS EN CASO DE QUE EXISTAN DUDAS EN LOS PROCESOS
NA
en efecto el cambio esta en nosotros, pero conocer las directrices es fundamental para asumir un rol y jugar un papel crucial en lo que cpm quiere lograr
ninguno
SEGUIR LA METODOLOGIA QUE SE ESTA VIENDO EN EL CURSO.
Un poco más
,
todo bien
"CONSIDERO EN EL TRANSCURSO DEL PROCESO NOS DAREMOS CUENTA SI ES NECESARIO AGREGAR ALGUNAS OTRAS PRECISIONES PARA EL EXITO DEL METODO, DE MOMENTO EL HACER LLEGAR LOS RECORDATORIOS SERA MUY BUENO PARA DE ALGUN MODO EL SOCIO SE SIENTA MAS COMPROMETIDO EN FOMALIZAR SU SOLICITUD, PUESTO QUE EN OCASIONES EN SUCURSAL POR TIEMPO NO LES LLAMAMOS A DIARIO PARA QUE EL SOCIO SE ENCUENTRE ACOMPAÑADO CON LOS REQUISITOS, Y EN OTRAS OCASIONES DEPENDE EL PERFIL DEL SOCIO TAMPOCO ES BUENO INSISTIR SOLO ESPERAR A DUDAS QUE EL  NOS EXTERNE.

                           GRACIAS"
TODO BIEN
N/A
.
.
bien
,,,,
ninguna
CURSO REVISADO
123
todo bien
POR EL MOMENTO NO TENGO ALGUNA DUDA, TODO IRA SOBRE LA MARCHA Y EN ESE MOMENTO ESPERO CONTAR CON EL APOYO PARA PODER ASESORARME
5
me parece muy buena estrategia para alcanzar la colocación de créditos, realizando el recordatorio a nuestros socios con la documentación.
..
NINGUNA
SEGUIR CAPACITANDOME
RESOLVER DUDAS Y PREGUNTAS
-----------------------
  NINGUNA
BRINDAREMOS EXPERIENCIAS AL SOCIO
BUEN CURSO
.
...
excelente informacion
TODO ESTUVO BIEN
TAL VEZ UNA CHARLA PRESENCIAL
LAS MAS OPTIMAS Y MAS EFICIENTES
LA INFORMACION FUE MUY CONGRUENTE
SIN COMENTARIOS
UN POCO MAS RESUMIDA LA INFORMACION
.
AGREGAR HERRAMIENTAS DE DIGITALIZACION Y ENVIO DE DOCUMENTOS POR MEDIOS DIGITALES.
.
los principales motivos o razones de cancelación, opciones para ofrecerle al socio, y que de esta manera la solicitud se lleve hasta la fase de instrumentación y entrega
.
BIEN
Todo esta Claramente explicado.
.
TODO BIEN :D
INFORMACION MUY IMPORTANTE, SIN EMBARGO POR TEMAS DE AUDIO Y TIEMPOS NO SE PUEDE VISUALIZAR BIEN LOS VIDEOS
El tema quedo muy claro, excelente presentacion con imagenes de los colaboradores.
saber que errores comunes podrían aparecer con esta nueva iniciativa
buen curso
todo muy explicado
con el tema se retroalimenta mucha la situacion de estos casos
BUEN TEMA
CONCER A FONDO EL TEMA ARA ASI PODERLO ENTENDER PARA DESPUES SABER EXPLICARLO POR SI FUERA EL CASO.
na
gracias
SOLO AHORITA EMPEZAR A ACOSTUMBRARNOS Y ADAPTARNOS, SOBRE LA MARCHA TENDREMOS QUE TRABAJAR
SE REQUIERE DE UNA MA YOR APERTURA PARA EL BURO DE CREDITO
OK
NADA DE MOMENTO
hasta el momento ninguno
ME PARECE IMPORTANTE SABER ALTERNATIVAS QUE NOS PUEDAN AYUDAR A CONCRETAR LAS SOLICITUDES DE CREDITO
GRACIAS POR LA ACTUALIZACION QUE NOS MUESTRA EL CURSO ES MUY INTERESANTE CONOCER EL IMPACTO QUE TIENE LA CANCELACION DE SOLICITUDES
NO
SIN COMENTARIOS
.
.
.
.
.
una explicación de manera presencial y operacional para captar de mejor forma lo que se plantea.
..................
na
VERLO EN ESCRITORIO PORQUE ESTOY CUBRIENDO CAJAS.
ninguno
.
EXCELENTE INFORMACION
.
gracias
kl-kl-kh
bien
N/A
excelente informacion
todo muy bien
.
GRACIAS
ninguna
CONOER MAS A FONDO EL PROGRAMA
EXCELENTE INFORMACION COMPAÑEROS
conocer muy a fondo los procesos para llevar a cabo un buen trabajo.
ok
hacer capacitacion presencial
Alternativas concretas para los socios
TODA LA INFORMACIÓN MUY CLARA, SE LES ENVIARA WHATSAPP Y CORREO RECORDANDOLES LA FECHA DE ENTREGA DE SU DOCUMENTACIÓN, PARA QUE LOS SOCIO ACUDAN A SUCURSAL A CONTINUAR CON SU SOLICITUD DE CRÉDITO.
que se de el apoyo entre todo el equipo para una mejora y además de que se brinde una asesoría clara al socio
es muy bueno innovar
sin comentarios
Excelente
SIN COMENTARIO
...
dar la informacion precisa a los socios.
QUE EN SUCURSALES SE HABILITEN ALGUNOS EQUIPOS DE COMUNICACION (CELULARES, TABLETS, LAPTOPS) DONDE PODAMOS CONSULTAR E IMPRIMIR DE MANERA MAS EFICIENTE DOCUMENTOS DE LOS SOCIOS, PARA DARLE SEGUIMIENTO A LAS SOLICITUDES DE SUS CREDITOS.
todo correcto
OK
ok
SIN COMENTARIOS
EXCELENTE TEMA
hay muchos detalles que quedan en duda, el curso no es lo suficientemente claro
gracias
hay informacion muy completaq en los pdf que nos comparten
h
informacvion clara y precisa para afrontar las necesidades de losmsocios brindandole una mejor atencion mediante los cambios efectuados en cpm
NINGUNA TODO EN ORDEN
EXCELENTE :)
n/a
SIN COMENTARIOS
ok
..
buen curso
.
na
BIEN
Creo que por ahora no
N/A
.
BUENO
El curso realmente no es curso, solo son plantillas con información y ya, no tiene nada de interactivo y solo se basa en leer
mayor enfoque y capacitación
SIN COMENTARIOS
ninguno todo es muy claro
.
sin comentarios
POR EL MOMENTO CONCIDERO QUE LA INFORMACION ES BUENA Y OPORTUNA YA QUE DE ESTA MANERA PODEMOS DIGERIR Y MANEJARLA DE FORMA CORRECTA Y SEGURA, YA CONFORME AVANCE PUDIESEN BAJAR MAS INFORMACIONES COMO ESTA
SOBRETODO ENTENDER TODOS LOS CAMBIOS QUE SE APROXIMAN Y ADAPTARME.
gracias
Ninguno
.
LA INFORMACION
estan muy bien las estrategias a implementar
MAS INFORMACION SOBRE LOS PASOS A SEGUIR
excelente curso
QUE ESTE TEMA VENGA EN EL MOS DE CREDITO
EL TENER MAYORES OPCIONES DE PRESTAMOS, COMO AGILIZAR LOS TRAMITES .
COMO SE IMPLEMENTARÁ LA ENTREGA DE DOCUMENTOS Y SI SERA NECESARIO DARLES SEGUIMIENTO CONTINUO
INFORMACION IMPORTANTE GRACIAS
BUENO
GRACIAS
.
OK
h
#NAME?
CONSIDERO QUE EN CRM TENEMOS LAS HERRAMIENTAS PARA PARA DETECTAR SI UNA SOLICIDUT ES VIABLE O NO, YO CREO QUE LO QUE NECESITAMOS ES PONERLE MUCHA ATENCION AL TRAMITE Y AGOTAR LOS RECURSO QUE ESTEN EN LAS POSIBILIDADES DEL SOCIO PARA SU TRAMITE, LO QUE NECESITAMOS ES SABER COMO TRATAR A LOS SOCIOS ( EMPATIA), AHI LA IMPORTANCIA DENUESTRO SERVICIO.
Todo bien por el momento.
INFORMACION MUY BUENA
conocer mas a detalle en que se basan para cancelar un creido
muy bien
sin comentarios
SIN COMENTARIOS
bien gracias
NA
OK
:)
todo me parece muy bien y muy claro
EN GENERAL TODO BIEN
claro y sencillo
CURSOS PRESENCIALES
.
UN CUSDO SOBRE BURO DE CREDITO
..
ninguna todo bien
ninguno
ME SIENTO CON LA CAPACIDAD PARA ORIENTAR AL SOCIO A CONTINUAR CON SU SOLICITUD
GRACIAS POR SEGUIRNOS CAPACITANDO
gracias
excelentes contenidos
x
lainformacion proporcinada fue clara y consisa
.
...........
ok
na
OK
EXCELENTE CURSO
dar la antencion correcta e informacion al socio tanto de su tramite como de la papeleria a entregar para no tenga una mala experiencia al faltarle algun documento y despejar las dudas del socio
gracias
QUE SE PUEDA CONSULTAR EL HISTORIAL DE CREDITOS PASADOS EN CRM, PORQUE ACTUALMENTE SOLO SE PUEDE CONSULTAR EN CRS, Y NP TODOS TENEMOS ACCESO A ESE PROGRAMA
todo ok
bien
n/a
por el momento nada, ya cuando este el programa si me surgen dudas lo checare con el jefe inmediato
NINGUNA
todo bien
SIN COMENTARIOS
oki
DURANTE LA VISUALIZACION DEL CURSO, IDENTIFIQUE QUE SE CUBREN TODAS LAS NECESIDADES
.
LA COOPERATIVA NOS HA BRINDADO HERRAMIENTAS NECESARIAS PARA ACTUALIZARNOS POCO A POCO Y PODER BRINDAR UN MEJOS SERVICIO AL SOCIO.
CURSOS PRSENCIALES
ok
TODO BIEN
ninguna por el momento
SIN COMENTARIOS
REALIZAR PROGRAMA DE REFORZAMIENTO MENSUAL O TRIMESTRAL
conocer mas sobre el programa
bien
la atencion de escuchar la necesidad del socio
BUENOS VIDEOS DE ENTENDIMIENTO
no aplica
TODO BIEN
todo esta bien
todo sobre la marcha
ninguno
-..
SIN COMENTARIOS
TODO EXCELENTE
TODO BIEN
--
  NINGUNA
ninguna
ESTE CURSO NO AYUDA BASTANTE A TENER CONCIENCIA DEL  BUEN TRATO QUE HAY QUE TENER CON EL SOCIO
Ser de alguna manera más prácticos, porque en SEI el proceso es más rápido y en sucursal nos ponen muchas trabas y eso también al socio lo confunde porque es más rápido en la app y no presencial.
NA
N/A
NINGUNA, TODO BIEN
...
SOLO MANTENER LA INFORMACION ACTUALIZADA.
.
.
sn
APRENDIZAJE
.
bueno
ADECUAR LA PARTE DE LA DOCUMENTACION SOLICITADA EL SOCIO YA QUE SUELE MUY RECUERRENTE QUE EL SOCIO SE QUEJE POR ESTE ASPECTO.
BUENA INFORMACION
.
ME DA GUSTO QUE SE ESTEN IMPLENTANDO ESTOS GRANDES CAMBIOS PARA MEJORAR EL SERVICIO PARA NUESTROS SOCIOS Y NO SOLO ESO SI NO QUE ELLOS TENGAN UNA EXPERENCIA DE EXCELENCIA EN NUESTRO SERVICIO Y QUE SE VAYAN CONTENTOS Y SOBRE TODO PUEDAN RECOMENDAR A CPM A OTRAS PERSONAS, ESTO LO HABLO PORQUE DESDE MI SUCURSAL EN PACHUCA A SIDO UN POCO DIFICIL LLEGAR A CONVENCER A LOS HIDALGUENSES SOBRE CPM Y SUS GRANDES BENEFICIOS Y SIENTO QUE ESTO NOS VA AYUDAR MUCHISIMO
INFORMACION MAS DETALLADA Y DINAMICA
ok
N/A
Me agrado mucho el curso
....
PUES LA INFORMACION RECIBIDA DEL CURSO ES MUY INTERESANTE YA QUE TE DA OTRA PERSPECTIVA PARA TENER PRESENTE
NINGUNA
OK
EL MATERIAL COMPARTIDO ESTA ENTENDIBLE PARA ENTENDER EL CURSO. LO QUE CREO QUE AYDARIA A COMPRENDER EL TEMA|, SERIA EL HACER SESIONES DE MANERA PRESENCIAL CON LOS COLABORADORES
.....
DE MUCHA AYUDA EL CONTENIDO DE ESTE CURSO
SIN COMENTARIOS
ninguna
SE QUE LA PRIORIDAD SON LAS SOLICITUDES VIABLES, MI DUDA ES SOBRE COMO SE CANCELARAN LAS SOLICITUDES NO VIABLES
El poder exponer ideas con los compañeros sobre una soicitud de crédito que sea un poco compleja y de esta manera el poder llegar a un acuerdo y poder asesorar bien al socio.
todo bien
muy buena informacion
NULL
TODO BIEN
EVITAR REPROCESOS
por el momento se entiende el objetivo de lo que se quiere lograr, todo entendido.
¿Qué pasa con las solicitudes donde el socio desea cancelar su solicitud en el momento? ¿Se podrán cancelar o quedarán abiertas?
  EXCELENTE LA NUEVA FORMA DE RECORDARLE A SOCIO LA ENTREGA DE SUS DOCUMENTOS PARA FORMALIZAR
N/A
na
ENVIAR CASOS DE SOLICITUDES Y QUE DOCUMENTOS ADICIONALES PUEDE PRESENTAR EL SOCIO.
:)
LO MAS NECESARIO QUE SE APEGUE A LA NORMATIVA
LA INFORMACION ES MUY CLARA Y PRECISA EN CUANTO A LO QUE SE REQUIERE, ES DE MANERA PERSONAL DONDE HAY QUE SEGUIR APRENDIENDO Y COMPRENDIENDO CADA CONCEPTO DE COMO APLICAR ESTA METODOLOGIA,SI BIEN LA MENERA DE APRENDER DE CADA COLABORADOR ES DIFERENTE , POR LO QUE HABLANDO EN MI CASO HAY SITUACIONES QUE ME SON UN POCO COMPLICADAS DE COMPRENDER
Gracias.
......
NINGUNA
seguir con las capacitaciones constantemente
todo bien
TODO BIEN
ninguno
MANTENER UNA BUENA COMUNICACION DENTRO DEL EQUIPO, ASI COMO ESTAR ATENTO A LOS POSIBLES ESCENARIOS CON CADA SOCIO PARA DE ESTA  MANERA BRINDAR MEJORES SOLUCIONES Y DISMINUIR ESTAS INCIDENCIAS
tener conocimiento y estar actualizados dia con dia
todo bien
guias y practicas
ENCONTRAR LA MANERA DE SOLICITAR MENOS DOCUMENTOS A LOS SOCIOS SI YA HAN COMPROBADO SUS INGRESOS Y SU ACTIVIDAD ECOOMICA NO HA CAMBIADO
ESTAR MAS AL PENDIENTE DE FECHAS ACORDADAS CON SOCIO PARA ENTREGA DE DOCUMENTOS Y ASI FINALIZAR LA SOLICTUD
excelente curso
NINGUNA GRACIAS
por el momento ninguna
.
todo bien
TODO BIEN
que hacer cuando socio solo requiera información sobre como seria si llegara a solicitar un crédito, es decir para saber que taza de referencia poner en los simuladores
BUEN CURSO MUY DINAMICO
ninguna
TODO BIEN
Estaría bien de que despues de terminar los curso se pudiera volver a consultar el material didactico
na
NA
TODO BIEN
cambios en el sistema o metodologia.
la informacion fue muy clara
UNAS RETROALIMENTACIÓN DE MANERA PRESENCIAL
GRACIAS
COMO APOYAR AL SOCIO CUANDO SU CREDITO NO ES VIABLE
adaptacion
todo excelente
por el momento nada, ya veremos en la implementación de lo aprendido que dudas surgen, de acorde a cada caso.
NA
conocer mas la normativa vigente para creditos
ninguna por el momento
SIN COMENTARIO
Considero que ninguna, solo es cuestión de analizarlas y con ello lograr el entendimiento para compartir la información a mis colaboradores.
LA INFORMACION PROPORCIONADA ES SUFICIENTE
me parece todo muy bien, muchas gracias
LAS VISTAS EN CRM PARA SEGUIMIENTO PUNTUAL.
TODO BIEN
EL TEMAS ES MUY COMPLETO Y CLARO
Gracias
.
sin comentarios
sin comentarios
De momento ninguna la capacitación fue excelente, con buena información  que nos ayudará a tener buenos resultados en la disminución de solicitudes canceladas.
pienso que la metodología es buena y se tiene que implementar para ir monitoreando los resultados y las metas proyectadas, de esa manera tener una retroalimentación en base a resultados sobre lo deseado.
Considero que con los cambios que se van aplicar a partir del 10 de febrero, nos dará la posibilidad de un mayor impacto de seguimiento con nuestros socios, asi mismo creo que el resultado se obtendrá del acompañamiento que le demos a estas iniciativas, de momento considero todo cambio es bueno, y mas enfocados en la actualidad que vivimos
ninguna solo seguir los procesos para evitar cancelaciones y asesorar al socio de manera profesional y que se valla con satisfacción con el servicio otorgado  gracias por la retroalimentcion
ninguna
todo bien
esta bien la implementacion de recordatorio por mnj, todo el proceso se hace por sucursal,
NINGUNA, EN REALIDAD NADA MAS ES DAR UNA BUENA ASESORIA A LOS SOCIOS.
sin comentarios
Por el momento considero tenemos lo necesario
gcs
todo en orden
INFORMACION DE SUMA IMPORTANCIA
Todo bien
.
BUEN CURSO
aplicar la metodologia
.
SE CONTARA CON LO REQUERIDO PARA REALIZAR UN BUEN SERVICIO A NUESTROS SOCIOS.
HASTA EL MOMENTO EL QUE SE NOS VAYAN COMPARTIENDO LOS CAMBIOS DE MANERA COMO SE VAYAN DANDO ES IMPORTANTE PARA NO ATIBORRARNOS DE INFORMACION Y PODER SER EFECTIVOS EN TODAS LAS ACTIVIDADES A REALIZAR
ninguno
NINGUNA ADICIONAL
,
MUY BIEN
por lo pronto no es necesario
Considero que la información que se nos ha otorgado puede coadyuvar para esta mejora que se pretende implementar.
todo bien
ninguno
Nuestro perfil como supervisores, en mi opinion requiere ser considerdos en todas las capacitaciones, para conocer a detalle los procesos, gracias
Se pueda homologar la información con el área de Capyc,o bien nos informen en que casos solicitar, ya que no es un requisito para el trámite de crédito, en ocaciones solicitan al socio documentación adicional, como flujos de efectivo en créditos con finalidades productivas.
SIN COMENTARIOS
Excelente
tenemos la informacion necesaria
SIN COMETARIO
Hasta el momento ninguna.
EXCELENTE CURSO Y ME PARECE MUY BIEN LAS HERRAMIENTAS PROPUESTAS PARA ESTAR EN MAYOR CONTACTO CON EL SOCIO Y QUE ESTE NO NOS PIERDA DE VISTA COMO OPCION DE INSTITUCION CREDITICIA CONFIABLE.
excelente tema
bien
considero que la transmisión de conocimiento a sido la adecuada, ya que no implica conocimientos adicionales a los que ya tenemos, aquí solo depende de que nosotros como colaboradores implementemos la iniciativa y brindemos ese servicio de calidad a nuestros socios.
Al estar parametrizado el sistema, la parte del analisis se va a ver afectada ya que si se cuenta con antecedentes no favorables no se va a poder cancelar la solicitud
Gracias por la informacion
sin comentario
muy bien
.
ninguna
Posiblemente un curso de crédito y normativa
ACTUALMETE PIENSO QUE MUCHOS PROSPECTOS A SOCIO NO INGRESAN A LA INSTITUCIÓN PORQUE BUSCAN UNA ALTERNATIVA DE TASA DE INTERÉS MAS BARATA QUE LAS QUE LES OFRECE ALGUNA OTRA INSTITUCIÓN Y COMO NO PODEMOS DARLES CON CERTEZA LA TASA QUE SE LE ESTARÁ COBRANDO DECIDEN NO INGRESAR
EXCELENTE INFORMACION
GRACIAS
Gracias
QUE DEJE REALIZAR CAMBIOS A TIPO DE FINALIDAD, SIN NECESIDAD DE REALIZAR ALGUN MANTENIMIENTO
No
ES  IMPORTANTE  CHECAR CUALES SON  SON LOSPUENTOS QUE TENERMOS QUE PBSERVAR PARA UNA BUENA COLOCACION Y UN AGIL SERVICIO ,  A NUESTRO SOCIO
.
de momento la información esta muy completa
gracias
muy interesante
MUY BIEN
.
.
PDF PARA LECTURA
todo bien
ENTRAR EN ACCION
ninguno
X
SOCIO LO REQUIERE
s/c
a
GRACIAS
.
Excelente cambio.
curso completado
muy importante toda la informacion
.
NOTIFICACIONES DE CAMBIOS RELEVANTES, PARA TENR LA MISMA COMPRENSION EL EQUIPO DE TRABAJO
POR EL MOMENTO ES INICIAR EL RETO A SU VEZ IR MEJORANDO LAS AREAS DE OPORTUNIDAD PARA MEJORES RESULTADOS Y BRINDAR UN MEJOR SERVICIO AL SOCIO Y PUBLICO EN GENERAL
n
LAS SOLICITUDES INVIABLES EN SUCURSAL LA VAMOS A PODER CANCELAR O SE CANCELARAN A LOS 30 DIAS?
  Gracias
todo bien
EFICIENTAR LOS PROCESOS PARA MI TRASFORMACION INTERNA
por el momento ninguna, todo bien.
gracias
SIN COMENTARIOS
NINGUNA
considero que esta muy bien reforzar que el socio es lo principal para Cpm.
.
:)
REVISAR EL PLAN DE TRABAJO CON EL JEFE INMEDIATO PARA ESTAR EVALUANDO LOS RESULTADOS
.
tener un celular asignado a sucursal debido en en muchas ocasiones el socio trae las nominas o comprobante de domicilio reciente en cel lo que afecta el tiempo de respuesta o servicio debido que el socio se le olvida y no las trae impresas y así con el celular las envía y se le puede apoyar para imprimirlas en sucursal, haciéndole hincapié que para una nueva solicitud y las  presente de forma impresa
gracias
Todo esta bien
ninguna
PODER HABILITAR QUE LOS SOCIOS PENDAN ENVIAR LOS COCUEMNTOS VÍA WHATS A LA SUCURSAL
buen material y tema
No nos han informado nada  como funcionara , ni sabiamos del curso  , apenas nos enteramos que teniamos un curso por que entramos a imprimir la nomina.   no tenemos ideas del cambio que habra.   Este curso nos dio una idea lo que se espera  la cual es  genial MUCHAS FELICIDADES  me encanta.   solo falta la practica   y tener centralizada la información para acudir a ella de manera rapida y fácil.  Hemos solicitado apoyo para que nos modifiquen los paneles en la solicitud rechazadas por capyc  las cuales no nos aparecen esto para dar una mejor experiencia al socio al buscar de manera rapia la solicitud.
ninguna
que fuera presencial o se dieran el tiempo real de explicar bien
gracias
.
gracias
Todo esta bien
ok
Buen curso
Más que conocimientos, como gerente de zona ya no tengo acceso al sistema CRM, salvo que esté en oficina de plaza, cosa que no es muy seguido, la actividad es más bien en sucursales, por lo que pediría se analice la posibilidad de brindar dicho acceso y estar en condiciones de monitorear todos los trámites que realizan las sucursales.
muy buen contenido
Nada, contamos con las herramientas para seguir los procesos
.
TODO BIEN
sin comentario
GRACIAS
ninguno
ok
.
muy buena iniciativa
sin comentarios
información importante
gracias
Informe de las solicitudes que han sido canceladas por esa metodología, para identificar la causa raíz.
Se pudiera tener un monto mas alto en los créditos preautorizados para que en éstas también se disminuyan las cancelaciones. Se evitarian reprocesos, se tendria una mejora en el idicador de colocación y se estaría dando un mejor servicio al socio, impactando tambien en el medio ambiente.
.
X
ok
Gracias por la información
Ninguno
Que dicha capacitacion la tengamos disponible como material de consulta
QUE SEA UN PROCESO MAS SENCILLO Y MAS BREVE
MUY ENTENDIBLE
EL PROGRAMA ES BUENO, SERÍA BUENO LLEVAR CONTROL DE LAS CANCELACIONES
ninguna
Ninguno.
Que las guias de procesos y normativa sea mas especifica y en cuanto a procesos una secuencia mas clara
Curso muy dinamico
es una cuestión de actitud para el cumplimiento de este tan importante proyecto
sin comnetario
todo bien saludos
.
.
gracias
sin comentarios
"*Ser claros y precisos al momento de solicitarle la documentacion al socio para evitar darle vueltas 
*Mencionarle que si no presenta la documentacion completa no se podra seguir con su tramite
*Darle el seguimientooportuno para concluir con el proceso"
poder de convencimiento y consientizacion
GRACIAS
gracias
todo muy bien
NO
Contar con las vistas y los datos para poder validar y con ello implementar mejores opciones para que las solicitudes de cancelacion disminuyan
IDENTIFICAR LAS VISTAS
.
gracias ;)
Ninguna
Por el momento encuentro completa la informacion
todo bien
sin comentarios
"ESTOY CONTENTA CON ESTA IMPLEMENTACION, ESPERO QUE LAS CANCELACIONES DE LAS SOLICITUDES DISMINUYAN FAVORABLEMENTE.
MEJORAR LA EXPERIENCIA DEL SOCIO EN LOS MOMENTOS DE VERDAD SERA POSIBLE. GRACIAS."
por el momento no se requiere de mayor información ya que esta muy completa la información recibida.
buen cambioi muchas gracias
.
que se cuente con la herramientas necerias para  continuar las solicitudes, tales como dande escargar documentacion que el socio lo trae digital, que los analista de una buena asesoria de esos va depender mucho
Excelente curso, muy buenos videos.
NINGUNO
Agradezco la informacion
un block de notas en la vista 360 persona física
Los resultados y avances de mi sucursal sobre este nuevo cambio.
ok
TODO ESTA MUY COMPLETO, LA INFORMACION
muy interesante
.
NA
.
Gracias por transmitir la información de la manera más accesible y oportuna
BIEN
TIENEN QUE DAR ESTOS TEMAS A TODO EL PERSONAL DE MANERA DIRECTA NO ATRAVEZ DE LOS TITULARES SERIA IMPORTANTE Y NECESARIO
.
ESTA BIEN
de momento ninguna
OK
m
todo bien
queda en duda como hacerle para pedir menos documentacion de ingresos si se esta solicitando lo que marca la normativa y basandonos en resultados de buro de credito
El material compartido refuerza lo explicado en la sesión
bien
buena inciativa
Gracias
Buen curso
todo muy bien concreto y correcto para la mejora en los procesos dentro de cpm
.
Habilitar correo a todos los analistas para recepcion de documentos seria lo mejor para el socio, ya que actualmente todo lo presentan digital.
BIEN
.
.
Vistas
NADA QUE MANIFESTAR
TOPS PARA CONVRETAR LA VENTA DE UN CREDITO
gracias
ninguno
vistas de informacion efectivas para supervision
ok
los cambios son bueno pero los lograremos con buena actitud, ser empáticos con nuestros socio la mayoría requieren su crédito por necesidad, ponerme en los zapatos y tratar como guste que traten siento que en la actualidad  ser han perdido valores
Buena información
.
...
.
.
ES UN GRAN AVANCE
LA INFORMACIÓN AYUDARÁ A LA IMPLEMENTACIÓN Y MEJOR ATENCIÓN DE LAS SOLICITUDES QUE EL SOCIO NOS PRESENTE, BUSCANDO EL CONCRETAR DENTRO DE LO POSIBLE LOS TRÁMITES, Y ESTOS SEAN OPORTUNOS.
.
todo claro
.
gracias
ninguna
gracias
.
ok
excelente curso
ninguno
NINGUNO
Con la información presentada es suficiente
Curso muy ameno e importante para nuestras funciones. Que bueno que se toma esta iniciativa para el seguimiento de oportunidades.
.
Ninguna
dispositivo u opcion para que el socio pueda hace llegar sus documentos de manera digital o bien en sucursal podamos imprimirlos
Con los cambios que se implementaran se beneficiara el proceso de seguimiento a los creditos.
Por el momento ninguna
.
Proporcionar documento con dudas y respuestas
Todo bien...
todo bien
la información fue clara y se despejaron dudas de momento no se requiere apoyo
TIEMPO : INVERSION DEL SOCIO PARA PODER ADQUIRIR UN PRODUCTO O SERVICIO Y PERSONALIZACION; CONOCE A TU SOCIO IDENTIFICA SU NECESIDAD Y OFRECE UNA SOLUCION ADECUADA
...
.
Con relación a las capacitaciones en sistema sería bueno que integraran a nuestros puestos como ente supervisores para contar con mayor conocimiento en la aplicación, y respecto al tema de la disminución es relevante ya que perdemos oportunidades colocar y ayudar a los socios
Gracias
.
CONFORME LA IMPLEMETACION VAN SURGIENDO DUDAS LAS CUALES NOS APOYAREMOS CON EL AREA ESPECIALIZADA PARA SU ATENCION
I
todo bien
.
.
5mentarios
ok
excelente
ninguna quedo claro
excelente curso
.
DE MOMENTO NINGUNO
Esta completo
x
La información viene muy completa y clara para poder bajárselas a los colaboradores.
Creo que la información proporcionada es muy clara y precisa.
NINGUNO
bien
ninguna toda la información muy clara.
La informacion presentada es clara, y entiendo el objetivo de ser mas eficiente en cada uno de los procesos que se realizan dia con dia en cpm, y especificamente en el proceso de credito, ya que es la medula espinal donde se geran los ingresos con los que hacemos frente a nuestras necesidades.
Buen curso
NADA QUE MANIFESTAR
en el curso recibi los elementos necesarios gracias
todo muy entendible por el momento no hay requerimientos.
por el momento considero que tengo el conocimiento que se ocupa
ok
tema claro
.
Se ira dando sobre la marcha
Gracias
el curso es de gran ayuda para evitar las cancelaciones de credito y se pueda tener una mayor colocacion.
sin comentario
NINGUNO
SE PUEDE SABER EL NUMERO DE CANCELACIONES  Y POR QUE EL SOCIO NO PROCEDE AL TERMINO  EN LA CUAL TENDRIAMOS  INFORMACION PARA ESTABLECER OTRAS ESTRATEGIAS AL MERCADO.
muy bien
:)
ninguno todo en orden
ninguno
EXCELENTE CURSO
TODO BIEN
excelente tema
DE MOMENTO NO SE IDENTIFICA ALGUNA NECESIDAD
.
Todo  esta claro
Gracias.
MANTENER ACTUALIZADA LA NORMATIVA
no
la informacion que se nos comparte es muy importante para la implementacion del proyecto
Los materiales están muy completos.
ok
ok
na
en orden
ME PARECE UNA BUENA ESTRATEGIA, YA QUE DISMINUIRÁ EL TIEMPO DE ATENCIÓN Y POR ENDE PODREMOS BRINDAR SERVICIO A MAYOR NÚMERO DE SOCIOS Y NO SOCIOS.
Ninguna
IRÁ SALIENDO EN LA PRÁCTICA
EXCELENTE CURSO.
Gracias por el apoyo y mantenernos capacitados a cualquier cambio !!
  EXCELENTE
bien
Ninguna
.
NINGUNA
es un cambio que viene a apoyar una mejor experiencia al socio
EXCELENTE INFORMACION
todo bien
mas informacion
.
NINGUNA, TODO MUY CLARO
SIN COMENTARIOS
no hay comentarios
POR EL MOMENTO TENEMOS LAS HERRAMIENTAS NECESARIAS PARA TENER EXITO EN E PROGRAMA
Las solicitudes aprobadas es un producto que se le ofrece al socio y se deberian de entregar con menor tasa de interes, por cuestion de las competencias que los tramites son mas agilez e inmediatos sin tantos requisitos.
.
GRACIAS Y ESTO SERAN DE GRAN APOYO YA QUE SE PODRA COLOCAR MAS LO CUAL AYUDARA PARA LOS NUMEROS DE SUCURSAL
todo bien
ede
los conocimientos adquiridos son de suma importacias para desarrollar los procesos de forma correcta y actulizarnos
hacer mas dinámica la entrega de información a nuestros socios, poder enviar información de simulación de pagos como antes, y poder agilizar el proceso de asesoria.
TODO MUY BIEN
Considero nos ayuda para reforzar conocimientos
TODO BIEN
INTERESANTE
Me quedo muy clara la iniciativa y pondré en practica lo aprendido.
información completa
completado
todo en orden
ES IMPORTANTE ACTUALIZARNOS CADA VEZ MAS.
ninguna
ninguna todo esta claro
x
todo bien
NINGUNO
i
EL CURSO ES DE MUCHA IMPORTANCIA
todo claro
d
Una nueva experiencia en tomar estos cursos
.
hay situcaciones en las que el socio quiere saber en cuanto le va quedar el pago ahi tambien deberia haber esa opciones que solo era tipo consulta por si se animaba mas adelante
solamente revisar mis actividades de solicitudes y brindar una buena asesoría, para a si evitar la cancelación de la misma
NADA QUE MANIFESTAR
Un curso presencial que motive a los colaboradores del trato al socio que les inyecte ese positivismo de buscar y lograr una mejora continua de trabajo en equipo, ya que al ser actividades que hacen en su rol se vuelve muy rutinario y de vez en cuando hace falta una motivacion asi, que entiendo se pudiera hacer con un curso para ellos y eficientar la excelente experiencia del socio.
MUY BUEN TEMA
muy buena información muy completa y motivadora
PROGRAMA RO IMPLEMENTAR CAPACITACION DIRECTA  A LOS ANALISTAS
.
ninguna
TODO ESTA BIEN GRACIAS
CURSO TOTALMENTE PRACTICO
todo ok
QUEDO ENTENDIDO
MUY BUENA INFORMACION
todo bien
sin comentarios
INFORMACION DE IMPORTANCIA PARA REALIZAR LAS ACTIVIDADES
N/A
mas que nada que en el area de capyc respeten tambien lo que se requiere como documentacion para tramite de credito en la normativa ya que ellos son los que en ocaiones piden documentacion adicional
Todo Bien
INFORMACION IMPORTANTE
Ninguna
ninguna
NINGUNA
.
ninguna por el momento
todo bien
Es simplemente dar una buena asesoría y cuando es o no viable un crédito
.
que nos puedan llegar asi como los mensajes preventivos y posteriores pero en correo a los responsables de sucursal, para tener esta info mas a la mano
EXCELENTE
IMPORTANTE
buen temA de importancia
todo esta claro
.
.
buena informacion
NINGUNA
gracias
considero que con la informacion que se ha proporcionado es suficiente, nos compartieron todo lo necesario para poder operar dentro este nuevo modelo
todo bien
NINGUNA
Estrategias, disminuir tasas de interes, que el socio pueda mandar documentación a un correo o tableta y poder descargasra en sucursal.
mejoras en el sistema
Gracias
gracias
GRACIAS
.
de momento nada, todo esta claro.
todo bien
Al momento es cuestion de implementarlos, el envio de mensajes de textos como recordatorios aportaran mas al compromiso del socio a continuar con su tramite y asi mismo nosotros al seguir brindando una asesoria clara y oportuna para disminuir el numero de solicitudes canceladas.
.
TODO BIEN
Que se les recomiende a mis compañeros analistas de crédito a que retomen las asesorías que hacen todos los días para que el socio regrese a  realizar su trámite, ya que veo que no le dan la importancia aún teniendo mucho tiempo libre para realizar esta esta estrategia tan sencilla. Gracias por un conocimiento más pata todos.
NINGUNA.
ME PARECE TODO MUY EXPLICITO,CONSIDERO NO TENER NECESIDAD DE ALGO EXTRA POR EL MOMENTO
CONSIDERO QUE HASTA Q YA ESTE EN MARCHA PODREMOS IDENTIFICAR LAS MEJORAS A REALIZAR
Guías claras y unificación de criterios de los cambios implementados
Tal vez una explicación personalizada por parte de jefes inmediatos, sumada a la información digital.
facil de comprender
Ninguna , considero que se tiene el conocimiento para disminuir el número de solicitudes aprobadas.
OK
SIN COMENTARIOS
LA INFORMACION ES SUFICIENTE
ninguna
SIN COMENTARIOS
LA INICIATIVA QUE SE IMPLEMENTARA ES BUENA PERO ES NECESARIO QUE EN SUCURSAL Y EN PLAZA LO LLEVEMOS A CABO YA QUE SE TIENEN OTRAS INDICACIONES DENTRO DE ELLAS ESTAN EL CANCELAR LAS SOLICITUDES QUE TENGAN MAS DE 7 DIAS, ES POR ELLO QUE EXISTEN MUCHAS CANCELACIONES
Excelentes practicas para el desarrollo de las actividades laborables.
ninguno
gracias
Pues como en todo proceso nuevo lo unico que se necesita es practica
ES BUENO CONOCER MAS SOBRE EL PRODUCTO QUE SE LES ESTARA OFRECIENDOA  A LOS SOCIOS SUS VENTAJAS Y DESVENTAJAS  Y ANALISAR SU CAPACIDAD DE PAGO.
ME PARECE UNA MEJORA EN NUESTRO SSERVICIO, YA QUE MUCHOS SOCIOS POR CUESTIONES LABORALES NO PUEDEN ACUDIR LUEGO A REALIZAR SUS DESEMBOLSOS
ninguna
todo claro
buen contenido
.
.
todo bien
todo bien
10
muy buenos los videos
EXCELENTE TEMA
SI EN LA MAS COMPLETA DISPOSICION PARA IMPLEMENTAR LOS CAMBIOS
ESTA ACTUALIZACION PERMITE UN MAYOR SEGUIMIENTO A SOLICITUDES
bien
todo claro y conciso
Ninguno
APEGARME DIA A DIA A MI NORMATIVA DE CREDITO Y COBRANZA
por el momento ninguna
bueno
NINGUNA, ME QUEDA CLARO QUE LO QUE PUEDO HACER ES HABLARLE A MIS SOCIO PARA DARLE SEGUIMIENTO A LA SOLICITUD PARA QUE SEA EL MENOR NUMERO DE CANCELACIONES
sin comentarios
.
muy interesante, ademas de siempre necesitamos ver de manera positiva todos los cambios ya que son muy buenos
.
sin  comentarios
todo correcto
DESDE QUE LLEGUE A LA COOPERATIVA DONDE ANTIGUAMENTE TRABAJE TRAIAMOS LA MENTALIDAD QUE ENMARCA ESTE CURSO HAN PASADO 11 AÑOS Y ME DA GUSTO QUE MUCHAS OBSERVACIONES QUE VEIA EN SUCURSAL POR FIN HAGAN ECO EN TODOS LOS COMPAÑEROS PARA LOGRAR SER MAS COMPETITIVOS Y RENTABLES
Se requieren cursos presenciales para poder realizar pruebas cercanas a la realidad.
GRACIAS
La información es clara y precisa.
se menciona muy poco el proceso se deberia aundar  mas en el
c
+
  TODO BIEN NINGUNA
CON LAS VISTAS
gracias
.
CAPACITARNOS MAS EN NORMATIVA YA QUE EN SU MAYORIA NOS HACE FALTA CONOCIMIENTOS RELACIONADO A LA NORMATIVA ADEMAS DE DAR SEGUMIENTO OPORTUNA A LAS SOLICITUDES
SIN COMENTARIOS
Que las causas de cancelacion de las solicitudes sean las que realmente el socio origine, flexibilidad real al momento que las solicitudes esten en capyc por que, en parte del informacion leida mencionan que se piden minimos requisitos, pero muchas veces en capyc rechazan las solicitudes por que a su criterio la documentacion no es minima y no tienen coherencia con lo que se quiere lograr con el socio.
GRACIAS
Ninguna esta clara la información, solo hay que llevarla a cabo, y asi se pueda lograr el objetivo para bien del socio y nuestra insitucion.
.
ninguna
Por el momento sin comentarios
sin comentarios
.
.
INFORMACION IMPORTANTE
capacitaciones preenciales no es lo mismo virtual
Ok
SER MAS CLAROS CON LOS SOCIOS, CON LA DOCUMENTACIÓN REQUERIDA Y DARLE OPCIONES PARA SATISFACER SUS NECESIDADES. Y BUEN SERVICIO.
guias prácticas
GRACIAS
excelente
SE REALIZO CURSO
ninguna
DE ACUERDO
SERIA BUENO IMPLEMENTAR UN CURSO DISEÑADO PARA ANALISTAS CON UN ENFOQE DE SERVICIO DE ESTA MANERA ENTENDERAN EL POR QUE SE REALIZAN ESTAS INICIATIVAS,
muy buen curso
CONSIDERO QUE  LA INFORMACION ES CLARA
ESTA BIEN QUE LOS HAGAN ASI GENERA MUY BUENA EXPERIENCIA Y APRENDISAJE
TODO MUY BIEN
Seguimiento del impacto de la imprementacion
ninguno
La información fue clara.
.
muy completa la info
Bnn curso
Por el momento no hay requerimientos
BIEN
gracias
.
0
todo bien
NINGUNA
ES MUY SUSTANCIAL CONOCER EL POR QUE DE LAS CANCELACIONES DE SOLICITUDES Y DARLES UN MAYOR SEGUIMIENTO PARA QUE EL SOCIO SE LLEVE UN SERVICIO COMPLETO QUE LE SEA SATISFACTORIO EN TODOS LOS ASPECTOS
NINGUNA
.
INFORMACION CLARA Y CONCRETA
BIEN
Todo completo
ninguna
na
BUENO
en la generación de reportes para su buen seguimiento y asi tambien mejorar la colocación
buen curso
.
OK
TODO BIEN
ninguno
todo bien
NINGUNA
buen curso
444
EXCELENTE
.....
OK TODO BIEN
.
OFRECER AL SOCIO PRODUCTOS ATRACTIVOS Y CON FACILIDAD DE ADQUISICION LOS CUALES NOS POSICIONARAN ANTE LA COMPETENCIA Y SE INCREMENTARAN LAS EXPERIENCIAS POSITIVAS A LOS SOCIO LOS CUALES NOS RECOMENDARN CON MAS SOCIOS.
exelente contenido
.
buen curso
CURSO RESUMIDO
Como se le puede apoyar a un socio que está bien en buró o que no tiene historial, pero que por su nivel de riesgo no se otorga?
  ACTUALMENTE NO CUENTO CON CRS Y NO PUEDO SABER SI EL SOCIO ESTA AL CORRIENTE CON SUS DEMAS CREDITOS TAMPOCO EL COMPORTAMIENTO DE PAGO ESPERO QUE ESTA NUEVA HERRAMIENTA SE PUEDA VER TODO COMO ANTES DE LA CONTINGENCIA TECNOLOGICA.
ninguna
EXCELENTE
capacitación presencial
es otro sistema? o esta implementado en crm
ninguna
.
.
.
.
ok
ESTA CLARO LO QUE SE PRETENDE HACER, ASI QUE TODO SERA EN APEGO A NORMATIVA
ok
SABER MAS DEL SISTEMA QUE IMPLEMENTARAN
.
TENEMOS LO NECESARIO PARA DISMINUIRLO
guías de apoyo para comprender la informacion.
EXCELENTE CURSO
ES BUENA OPCION TENER UN POCO MAS DE TIEMPO EN LAS SOLICITUDES PORQUE HAY SOCIOS QUE PASAN A PEDIR INFORMACION PERO SON DE COMUNIDADES RETIRADAS QUE NECESITAN TIEMPO PARA REUNIR SU DOCUMENTACION NECESARIA.
.
CREO QUE CON EL MATERIAL PRESENTADO SE CUBRE LAS NECESIDADES DEL PROGRAMA SMART CORE
ok
z
CONOCIMIENTO O INFORMACION NO, SOLAMENTE QUE A VECES LE SOLICITAN DEMAS DOCUMENTOS AL SOCIO EL AREA DE CREDITO Y ES CUANDO EL SOCIO DESISTE DE CONTINUAR CON EL PROCESO DE CREDITO (EN LOS CASOS DE INGRESOS VARIABLES), LOS CREDITO QUE VAYAN APOYADOS DE SUCURSAL Y QUE CUENTEN CON BUEN HISTORIAL E INGRESOS COMPROBADOS DEBERIAN SER MAS FLEXIBLES CON LA DOCUMENTACION REQUERIDA.
gracias
GRACIAS
NINGUNO
...
todo bien
Sin comentarios
que pasara cuando la cancelacion se de por falta de documentacion y o requisitos, y en cambio si realiza el tramite por la app si se los reciban ?
  TODA LA INFORMACION PRESENTADA ES CLARA, POR LO PRONTO CON EL MATERIAL DE APOYO LOS PDF ES CLARO
En lo particular las capacitación es buena entendible sin embargo se que sale del tema pero nuestro sistema  es muy malo y para poder compartir con es muy importante el servicio que se bri da en sucursal basado en la agilidad y la rapidez sin embargo con nuestro sistema se quedan  cortos esos propósitos
CANCELACION EN AUTOMATICO DESPUES DE LOS 30 DIAS
.
ok
EXCELENTE CURO
N/A
nada que manifestar
gracias
.
"*supervisión de los superiores
*capacitación constante por los cambios de normativa y sistema"
.
MEJORAR EL TIEMPO DE RESPUESTA POR LA INSTANCIA FACULTADA
.
d
ninmguno
es de gran ayuda lo reforzamientos
subtitulos en cada video
gracias
todo claro
DE MOMENTO NINGUNA...
EXCELENTE INFORMACION COMPLETA
todo muy bien
TEMAS DE GRAN IMPORTANCIA PARA DESARROLLO DE PROYECTOS NUEVOS
ok
creo que la información brindada en el curso es completa
.
.
gracias
ESPERMOS CON ESTOS CAMBIO Y COMPROMISO POR PARTE DE TODOS LOS COLABORADORES SUMEMOS PARA MEJORAR LA COLOCACION Y PODER CUMPLIR CON LOS INDICACADORES DE LA SUCURSAL, PERO SOBRE TODO DARLE AL SOCIO LO QUE NECESITA PARA MEJOR SU CALIDAD DE VIDA.
seria bueno implementar una capacitacion con practica donde se peudan despejar todas nuestras dudas
muy buen curso
todo bien
LA INSFORMACION ES BUENA PARA ACTUALIZAR Y LOS PROCESOS SEAN MAS RAPIDOS
MUY BUENA LA PRESENTACION Y NOS AYUDA A PODER REFRESCAR NUESTROS CONOCIMIENTOS
.
NO SE REQUIERE MAS YA QUE ESTA  HERRAMIENTA APORTA LA INFORMACION NECESARIA
Buena informacion, tener guias actualizadas sobre los procesos
OK
.
Gracias por la informacion
sabor los procesos de cada producto , asi como la documentacion que se requjiere en cada ingreso .
QUE EN CRM SE PUEDE VISUALIZAR UNA ALERTA PARA LA SOLICITUD QUE ESTA PROXIMA A CUMPLIR SU VIGENCIA EN EN ENTREGA DE DOCUMENTACION Y EL SOCIO NO LA HA PRESENTADO PARA IDENTIFICAR MAS PRONTA Y RAPIDAMENTE Y PODER LLAMARLE PARA RECORDARLE DE FORMA PERSONAL
ok
Los materiales compartido  traen la  información   suficiente  sobre los conocimientos  para  las cancelaciones a las solicitudes, considero  que es clara la información.
EXCELENTE INFORMACION
muy bien
muy buena lectura para mejorar el servicio y que las solicitudes sean la menos que se cancelen
ALGUNAS SOLICITUDES DE METODOLOGIA DE CONSUMO SE CANCELAN DERIVADO DEL TIEMPO QUE TARDA EN APROBARSE NORMALMENTE SON DE 3 A 8 DIAS, EL SOCIO AVECES YA NO OCUPA EL RECURSO PORQUE YA SOLVENTO SU NECESIDAD MAS RAPIDA EN OTRA INSTITUCION
todo muy claro y la informacion es exelente
.
.
OK
Con la informacion que se ha proporcionado hasta el momento es suficiente
excelente información
X
GRACIAS
.
ok
OK
z
muy buena informacion
muy buen tema
ES BUENO TODO
muchas gracias
ninguno
ES IMPORTANTE ESTAR PREPARADOS PARA TODOS LOS CAMBIOS, GENERAR EXPECTATIVAS POSITIVAS ASI COMO EXPERIENCIAS A LOS SOCIOS  PARA QUE SE ACOSTUMBREN A SENTIR UN SERVICIO SUFICIENTE Y ACORDE A SUS NECESIDADES, UNA BUENA CAPACITACION PRESENCIAL  DE MANERA PRACTICA DE COMO ATENDER DE LA MEJOR MANERA AL SOCIO SERIA MUY COMODA Y PROACTIVA
SIN COMENTARIOS
por ahorita la informacion esta clara
na
NINGUNO
GRACIAS POR EL MATERIAL DE APOYO Y LOS VIDEOS DE INFORMACION
INFORMACION COMPLETA
bien
buena implementación
sin comentarios
GRACIAS POR LA INFORMACION
.
.
.
.
gracias
...
CREO QUE HASTA AHORA LOS CONOCIMIENTOS HAN SIDO BUENOS SOLO FALTA LLEVARLOS MAS A LA PRACTICA PARA QUE LOS SOCIOS SE SIENTAN AUN MAS CONTENTOS CON CPM Y ASI PODER TENER MAS COLOCACION
excelente conforme vaya avanzando esta implementacion surgiran las dudas.
sin comentarios
bien
Por ahora es información suficiente, sobre la marcha irán saliendo requerimientos de información adicional.
ninguna
NINGUNA
"AGILIDAD EN SISTEMA
MEJORA CONTINUA Y CAPACITACIÓN"
NINGUNO
gracias
APLICAR LOS CONOCIMNIENTOS ADQUIRIDOS EN ESTE CURSO
OK
excelente informacion
sin
COMUNICACION Y EXPANSION DEL PROGAMA SOBRE LA INICIATIVA
BIEN
...
buen contenido
sin comentarios
.
BIEN
NINGUNO
.
LAS CANCELACIONES EN AUTOMATICO Y LAS NOTIFICACIONES QUE SE LE HACE AL SOCIO. GRACIAS
PROCESOS
TODO BIEN HASTA AHORA
DE MOMENTO TODO EXCELENTE
sin comentarios
.
todo bien :)
pienso que por lo pronto solo necesito poner en practica lo que especifica el curso para poder tener el cambio que se requiere.
BIEN
excelente propuesta para evitar las cancelaciones
NA
sin comentarios
NINGUNO
GRACIAS POR LA INFORMACION COMPARTIDA, SERA DE GRAN AYUDA
bien
estaré atenta a darle seguimiento a las precalificaciones viables.
.
SIN COMENTARIOS
EXCELENTE INFORMACION
..
QUE NOS ESTEN ENVIADO LA INFORMACIÓN DE LOS CAMBIOS
ME GUSTO EL CURSO GRACIAS
Que los requisitos solicitados a los socios se simplifiquen para asi poder llegar a colocar el credito. Gracias.
SE REVISA EL MERCADO DE CREDITOS PARA DAR ATENCION A LAS SOLICITUDES VIABLES U OFRECER ALTERNATIVAS, CON UNA SANA COLOCACION PRIMORDIALMENTE
bien
TENER TODA LA INFORMACION CORRESPONDIENTE CON PRECISIONES ASI COMO ESTOS CURSOS
TODO MUY CLARO Y ESPECIFICO
"REALIZAR DESDE EL INICIO UNA ASESORIA CLARA Y PRECISA
ACORDAR UNA FECHA PARA LA ENTREGA DE DOCUMENTOS
EN CASO DUDAS EL SOCIO PUEDE LLAMAR A SUCURSAL
MARCAR EL CHECK DE DOCUMENTOS
TENER SIEMPRE BUENA ACTITUD EN EL SERVICIO"
BUEN  CURSO
gracias por la retroalimentacion
TODO BIEN, BUEN MATERIAL
EXECELETE CURSO
POR EL MOMENTO TODO MUY BIEN
.
ENTENDER BIEN LOS CAMBIOS PARA ASI PONERLOS EN PRATICA
N/A
la informacion es clara
OK
muy buena informacion
INFORMACION PARA UN MEJOR CONTROL DE LAS SOLICITUDES PARA CREDITO
buena informacion
"EL CURSO FUE MUY PRECISO. 
EXISTE UNA GRAN INFORMACION ACERCA DE ESTA TEMA.
CONTINUAR CON CURSOS PARA LA ACTUALIZACION DE INFORMACION"
ninguna
NINGUNO
EXCELENTE
OK
.
.
NINGUNO
Considero se cuenta con la informacion necesaria
sin comentarios gracias
sin comentarios
.
SIN COMENTARIOS
ENTENDIBLE
BUEN CURSO
buen curso
n/a
TODO BIEN
saber sobre la nesecidad de los socios y para darle un seguimiento oportuno y brindarle todo nuestro conocimiento para que este reforzada la informacion
excelente info
no tengo ninguna duda sobre el proceso.
:)
MUY BIEN EXPLICADO
EXCELENTE
gracias
EXCELENTE INFORMACION
CRITERIOS .
Todo está claro
MAYOR COMPROMISO DE PARTE DE LOS JI AL MOMENTO DE LA COMUNICACION DE LA INFORMACION
.
.
ACTUALIZARME Y ESTUDIAR LOS CMBIO NORMATIVOS
Todo bien
ok
TODO BIEN
BUEN CURSO
EXCELENTE INFORMACION
EN EL CURSO ME ENSEÑO A IMPLEMENTAR LA INICIATIVA DE DISMINUIR EL NUMERO DE CANCELACIONES EN SOLICITUDES APROBADAS POR LA METODOLOGIA
ninguno
.
es importante conocer y estar actualizado para mejorar el servicio al socio
revisar nuevamente el motivo de cancelación y brindarle una asesoría con el finde cubrir sus necesidades del socio
ejemplos
Me parece muy útil esta información , para poner en practica
por el momento todo bien
muy completo
buen curso
A MI CONSIDERACION CREO QUE HACE MUCHA FALTA DE INFORMACION EN CUSTION AL CREDITYO HIPOTECARIO A SER UN POCO MAS ESPECIFICOS EN CUESTION A DOCUMENTACION EN GENERAL CON TODOS LOS CREDITOS Y PROFESIONES QUE EXISTEN MUCHOS NO SON FORMALES Y AVECES ES OMPLICADO PARA EL SOCIO CUMPLIR CON LOS COMPROBANTES DE INGRESOS.
excelente informacion
.
curso de facil compresión e interasante
.
.
la guía de servicio para seguimiento a solicitudes de créditos viables
IMPLEMENTAR UN TALLER CON EJEMPLOS ESPECIFICOS
tener algunas guías de trabajo  para poder aplicarlas
BIEN
Bien
NO REQUIERO
que se pueda dar mayor agilidad en custion de las resoluciones de nuestras solicitudes enviadas
NA
NA
me parece muy buena estrategia para minimizar creditos cancelados
POR EL MOMENTO NINGUNO
es funcional y de aprendizaje el curso
MUCHAS GRACIAS POR LA INFORMACION LA CUAL SERA DE MUCHA AYUDA PARA MEJORAR EL SERVICIO A NUESTROS SOCIOS.
buen curso
EXCELENTE
bien
posteriormente ampliar masa la capacitacion
NINGUNA, ES CUESTION DE IMPLEMENTAR LAS ACCIONES RECOMENDADAS PARA LOGRAR QUE LAS SOLICITUDES CANCELADAS DISMINUYAN
MAS INFORMACION DETALLA SOBRE LOS PASOS A TRABAJAR
Documentación correcta unificada
QUE SE PONGA EN PRACTICA PARA PÓDER ASI VER LOS RESULTADOS DEL PROGRAMA
nA
TODO BIEN
NINGUNO
completo
TODO BIEN
poner en practica mi cambio de emociones para poder tener mejores actitudes y de esa manera tratar de la manera decuada al socio y  echarle muchas ganas.
gracias por la información
X
ME PARECE MUY BIEN SEGUIR APRENDIENDO TEMAS NUEVOS
PRACTICA
Ninguna
ninguno
TODO BIEN.
Compartir los casos prácticos aplicados en el programa SMART CORE con más ejemplos, para poder abarcar diferentes situaciones.
CONSIDERO QUE SE ME PROPORCIONO TODA LA INFORMACION RESPECTO AL TEMA.
UNICAMENTE LA RETROALIMENTACION Y LA PRACTICA EN ESTAS SOLICITUDES YO CREO QUE ES LO QUE NOS LLEVARA A ENTENDERLO Y REALIZARLO AL 100%
estoy capacitada
ninguno
Muy bien...
.
CREO QUE CONFORME A LAS SITUACIONES QUE SE VAYAN PRESENTANDO CON LOS SOCIO PODRE SABER QUE CONOCIMIENTO REQUIERO FORTALECER  MAS
sin comentarios
El curso me pareció muy bueno, ya que ahí vemos el impacto que tiene la camcelación de las solicitudes y el motivo de las causas, de tal forma que asi se podrá tomar alguna solución para poder cancelar tantas.
na
POR EL MOMENTO LOS CONOCIMIENTOS QUE SE TIENEN SOBRE ESTA INICIATIVA ESTAN CLAROS, DURANTE EL PROCESO VEREMOS QUE DUDAS NOS SURGEN Y COMO PODEMOS ACLARARLAS
TRABAJAR EN LA FORMA EN LA QUE SE LE PROPORCIONAN LOS REQUISITOS AL SOCIO, ADEMAS DE REFORZAR LAS OPCIONES QUE TIENE EL SOCIO AL CUAL LE HA SIDO DENEDAGO EL CREDITO
GRACIAS.
Todo el material muy entendible, muy buena aportacion para el seguimiento oportuno de las solicitudes y con ello apoyar mayormente a nuestros socios
por el momento ninguna, esta claro el objetivo que se tiene el de lograr un porcentaje de colocación de ese volumen que actualmente se cancelan
todo bien por el momento
BUEN CURSO
En particular, ¿cómo se estarán llevando a cabo las cancelaciones de solicitudes de créditos que desde un inicio en la asesoría por medio de historial interno sabemos que no son viables, pero aun así el socio se aferra a que se le levante el trámite completo de formalización? Argumentando que de no ser así, expondrá su respectiva queja.
gracias
.
me parece excelente la implementacion que tendremos
LA MAS QUE SE PUEDA
LAS QUE ESTEN EN POSIBILIDAD DE PODER ADQUIRIR
N/A
SIN COMENTARIOS
v
.
todo bien
NA
"* quien enviará mensajes de SEI y de sucursal para recordar pendientes de papelería en solicitudes de crédito ???
*sobre cancelaciones de pre-solicitudes de los  productos  CREDINAMICO Y PERSONAL... por que solo esas y no también el productivo ???
*por que ya no se acepta que si el socio sale con error de datos en PRECALIFICACION,, PEREO YA TIENE BUEN HISTORIA INTERNO Y ANTIGUEDAD COMO SOCIO SE LE PUEDA HACER NUEVAMENTE PRESTAMO ???
Y QUIEN ESTARÁ ENVIANDO LOS CORREOS A LOS SOCIOS PARA RECORDAR REQUISITOS  ???
(((Y SOLO COMO COMENTARIO... HOY ESTUVO TODAVIA MAS LENTO EL CURSO,, YA QUE NO ME DEJÓ ABRIR OTRAS VENTANAS)))"
GLOSARIOS MAS ESPECIFICOS
.
sin comentarios
ninguna
Gracias
Muy interesante
Sin comentarios
excelente
ES IMPORTANTE TOMAR EN CUENTA LA RECOMENDACIONES QUE NOS HACEN YA QUE CON ESTO SE VERA REFLEJADO EN EL SERVICIO QUE SE LE OFRECE AL SOCIO
NINGUNO
Mejoras en el aplicativo actual CRM 365, constantemente tiene lentitud
buen curso
de momento considero que la información es suficiente
muy entendible.
.
gracias
Gracias
éxito
Gracias por la información
MUY COMPLETO EL CURSO
Seguimiento.
LA DISMINUCION DE DOCUMENTACION SOLICITADA POR EL AREA DE CAPYC, YA QUE EN OCACIONES SOLICITA DOCUMENTACION INNECESARIA , Y SOBRETODO, VALORAR LA ZONA DE CADA SUCURSAL , YA QUE NO EN TODOS LOS ESTADOS, NI EN TODAS LA ZONAS ES LO MISMA , TAMPOCO ES LA MISMA FORMA DE COMERCIALIZACION Y TRABAJO. GRACIAS .
.
Sin comentarios
na
Todo cmabio es bueno
Excelente opcion
DAR BIEN LA INFORMACION A NUESTROS SOBRE LOS REQUISITOS Y EXPLICARLES  CLARAMENTE LA INFORMACION A NUESTROS SOCIO
Todo muy bien
En los créditos productivos aplicara esta misma metodología en un futuro
GHH
GRACIAS
INFORMACION MUY IMPORTANTE QUE NOS AYUDARA  EN NUESTRAS ACTIVIDADES DIARIAS MEJORANDO LA EXPERIENCIA DEL SERVICIO AL SOCIO
información relevante para mejorar la atención al socio
GRACIAS
gracias
ok
BIEN
na
interesante
creo que la capacitacion es muy completa y sera de gran utilidad par cumplir con la colocacion
.
Todo bien
Considero que la informacion, herramienta proporcionada son adecuadas para el logro de cumplimiento de la iniciativa, solo tenemos que aplicarla correctamente.
cambios significativos
.
NINGUNA
Ninguna
INFORMACION MUY CLARA.
ES IMPORTANTE DISMINUIR EL NUMERO DE CANCELACIONES DE SOLICITUDES SIN INCREMENTAR A CARTERA MOROSA Y VENCIDA CON TAL DE COLOCAR EL MAYOR NUMERO DE SOLICITUDES, TENER CUIDADO CON ESO, SI SE CANCELAN ES POR ALGO PERO EN FIN
.
POR EL MOMENTO TODO ME QUEDO CLARO Y CREO QUE LO PUEDO REALIZAR
PUES LA TEORIA SE TIENE Y LA COMPRENDO INICIANDO EL PROGRAMA  EN SISTEMA OPERANDOLO TAL VEZ SURJA ALGUNA NECESIDAD
Ninguna por el momento.
por el momento ninguno gracias
gracias por la informacion
.
GRACIAS
gracias
sin comentarios
Por el momento me ha sido suficiente la información que se me ha proporcionado.
.
.
.
Todo bien gracias
NINGUNA
LOS TEMAS VISTOS SN DE MUCHA UTILIDAD, DE NOSOTROS DEPENDE DE RETROALIMENTACIÓN PARA SEGUIR DANDO UNA BUENA ASESORÍA
Hasta el momento con la entregada es suficiente
todo en orden
GUIAS DE LOS PROCESOS PARA REALIZAR CORRECTAMENTE DICHO PROCESO
MUY BUENA INFORMACIÓN
ninguno
LAS CAPACIOTACIONES DEBERIAN SER PRECENCIALES  NO EN LINEA PARA MEJOR APRENDISAJE
Buena información
MENOS REQUISITOS
hasta el momento ninguna
todo muy bien
Que pasa con el tiempo de cancelacion de  las solicitudes de los creditos que no son personal y credinamicos
gracias!
  TODO BIEN
SE LLEVA ACABO EL CURSO, EN EL CUAL ESTA MUY CLARO Y ESPECIFICO EL PROCEDER Y ACTUAR (CONTROL DE EMOCIONES, EMPATÍA SER EL VÍNCULO ENTRE SOCIO Y CPM PARA SATISFACER LAS NECESIDADES DE NUESTROS SOCIOS GENERANDO UNA EXPERIENCIA ALTAMENTE SATISFACTORIA) ASÍ COMO LAS HERRAMIENTAS TECNOLÓGICAS QUE  TENDREMOS PARA DESARROLLAR DE MANERA SATISFACTORIA NUESTRA ATENCION Y A SOCIO Y PÚBLIC EN GENERAL  ( SOCIOS POTENCIALES )
INFORMACION CLARA Y ENTENDIBLE.
Por el momento no encuentro alguna necesidad sobre el tema en mención
Dentro de plaza Puebla se llegan  a cancelar debidoa que jefa de credito nos presiona mucho con que deben entregarse, pero hay varios casos donde el socio no puede venir de inmediato, sin embargo se opta por cancelarlas por la presion de la jefa de credito
NINGUNA
que se pretende medir con la disminución de cancelación en las solicitudes.
Considero que contamos con la información suficiente para disminuir el numero de cancelaciones
POR EL MOMENTO CONSIDERO QUE LA INFORMACION COMPARTIDA ES SUFICIENTE
HASTA EL MOMENTO HA SIDO CLARO.
GENERAR LA REPORTERIA DE CANCELACIONES PARA SEGUIMIENTO YA QUE MI PUESTO ES DE SUPERVISIÓN.
LA INFORMACIÓN COMPARTIDA ES SUFICIENTE
Sin comentarios.
De momento considero todo está bien
.
ninguno
GRACIAS
Considero que debe de existir una excepción para cancelar las solicitudes cuando el socio lo desea hacer por la aplicación o viceversa, ya que es algo de la flexibilidad para dar le un buen servicio al socio en base a sus necesidades.
seguirnos capacitando en los procesos de crédito, compartirnos todos los cambios en tiempo y forma, que Capyc también tenga una apertura en apoyo al socio para integración de información y soportes en el crédito.
Todo correcto.
GRACIAS
Por el momento todo me queda claro
EL MATERIAL ES MUY BUENO
ESTOY CONCIENTE QUE LA ASESORIA Y COMUNICACION QUE SE TENGA CON EL SOCIO, ES VITAL PARA EVITAR LAS CANCELACIONES DE LAS SOLICITUDES
POR EL MOMENTO TODO ESTA CLARO SOBRE LA MARCHA IRAN SURGIENDO DUDAS
todo este material nos servira para concientizar al analista sobre la importancia del servicio al socio y poder dar seguiminento a todos los tramites.
MUY BUENA INFORMACION
información especifica de como localizar las vistas generadas para el seguimiento de solicitudes
** CONTAR SIEMPRE CON ESTAS CAPACITACIONES E INFORMACION PARA CONOCIMIENTO DE LOS NUEVOS CAMBIOS SON BUENOS
excelente servicio
excelente informacion
muy interesante el tema
Comentar y preguntar si toda esta información que se reviso en el curso es de conocimiento de los compañeros de CAPYC, ya que como se reviso una de las causas de desistimiento del socio es la solicitud excesiva de documentación en cuanto a sus ingresos, actividad, etc., Hemos tenido casos en los que de parte de sucursal se le piden documentos acordes y que marca normativa pero CAPYC solicita mas información y documentación, lo cual retrasa el tiempo de respuesta y que en algunos casos el socio decida cancelar el tramite, esto en casos donde se observa viable apoyar al socio, en el entendido que si existen casos que si es necesarios pedir mayor soporte documental.
sin comentarios
GRACIAS
.
excelente informacion
o.k.
ES MUY TARDADO DESCARGAR PDF CON INFORMACION
SIN COMENTARIOS
VIVIMOS  EN UN MUNDO  DE PRISAS Y EL SOCIO  NO TIENE  EL TIEMPO  SUFICENTE  PARA DURAR EN ESPERA PORQUE  SUS COMPROMISIS  LABORALES , PROFESIONALES  NO SE  LO PERMITEN  Y CPM NO LES  ESTA HACIENDO  NINGUN FAVOR ENTIENDANLO BIEN, LA  BUROCRACIA EN TRAMITES , PROCESOS  REPETITIVOS  Y POCO PERSONAL  EN ATENCION NO ES  SUFICIENTE NESECITAMOS  MAS  ANALISTAS  DE  CREDITO ,DE QUE  SIRVE TENER MUCHO MERCADO Y  NO TENEMOS  EL PERSONAL SUFICENTE  PARA  REALIZARLO ,  Y OJO NO ES PRESIONANDO AL PERSONAL A  DAR MAS , ES  NADA  MAS  SER CONGRUENTES
En caso de que el socio no elija el producto y desee otro me permita analizar por otro producto, ya que actualmente si esta una aseria abierta vigente no me permite realizar otra sino se cancela.
.
QUE EL SEGUIMIENTO SEA CONSTANTE
excelente curso
Todo bien
BUENA ESTRATEGIAS
Gracias
TODO BIEN
excelente informacion
.
excelentes cambios
todo claro
Excelente curso, me parece importante que con esta programa y sus mediciones nos dan a saber los índices de colocación que estamos perdiendo..
LLEVAR TODA LA INFORMACIÓN A LA PRACTICA POR CADA UNO DE LOS INTEGRANTES DE SUCURSAL
SEGUIR EN AUTOESTUDIO, ORIENTACIÓN A LOS REQUERIMIENTOS DEL SOCIO, ASÍ COMO BRINDAR UNA ASESORIA CLARA Y QUE EL SOCIO VEA UNA MEJORA EN LOS TIEMPOS DE ATENCIÓN Y CON ELLO CONTRIBUYA AL RESULTADO DE LOS INDICADORES
TODO EN ORDEN
.
INFORMACION IMPORTANTE
ok
todo bien
muy interesante
excelente
ACTITUD EN EL SERVICIO
EN LAS GUIAS QUE NOS PROPORCIONAN VIENE TODA LA INFORMACION, HASTA EL MOMENTO NO TENGO DUDAS
Ninguna
.
considero que todo esta bien explicado solo hay que poner en accion
gracias
.
Gracias por mantenernos actualizados
POR EL MOMENTO HA QUEDADO CLARO LA INICIATIVA
Considero que debemos complementar este conocimiento con una constante retroalimentación o actividades en tema de habilidades que mejoren la calidad del servicio.
Ninguna
Considero que estas iniciativas no solo se enfoque en 2 productos, sino en general.
Reporteria que nos permita dar segumiento con aquellos socios digamos rescatables para poder realizar un nuevo trámite de crédito, es decir, aquellos que cuentan con los elementos necesarios para tramite de crédito.
es importante que esto empieza de arriba hacia abajo para que fusione.
Por el momento todo bien
Ser competitivos con una transformación interna que permita un impacto a nuestros socios en el uso de la tecnología
sc
esta muy bien esta nueva iniciativa e CPM para que nuestro indicador de colocación sea mas efectivo.
ok
toda la informacion fue muy clara
si
No le veo problema a la cancelacion, le veo problema a los requisitos en fisico, teniendo tantos portales con la inforacion de los socios
todo en orden
por el momento todo el conocimiento es claro
.
gracias por el tema y videos compartidos
ninguna
ningun comentario
Dar seguimiento a los colaboradores para que den la informacion correcta y que vean que el socio esta convencido de su tramite
ninguno
B
De momento ninguna.
Gracias
ninguna
INFORMACION RELEVANTE
ME GUSTO COMONOS COMPARTIERON LA IFNORMACION ,  LO CUAL SIN DUAD AYUDARA A MEJORAR
EN UNA EXCELENTE HERRAMIENTA MAS
muy importante para nuestros socio
excelenta informacion, gracias.
ninguno el curso es claro
gracias
.
LA DISPOSICION DEL PERSONAL Y APLICACION CORRECTA  DE PROCESOS ES MUY IMPORTANTE PARA AVANZAR EN LOS CAMBIOS
sin comentarios
sin comentarios
na
Todo lo relacionado para realizar mi trabajo y apoyar a mi equipo de trabajo exitosamente.
.
c
NINGUNO
.
SOLO ME SURGIRIA HASTA QUE SALGA EN PROCESO EN VIVO
muy buen curso
Capacitación de reforzamiento sobre  la metodología, para seguir dando una mejor información integra y satisfacer de mejor manera la necesidad del socio de acuerdo a la asesoría que se le brinde.
TODO EN ORDEN
.
EVALUACION DE ALTERNATIVAS
ninguno, porque las cancelaciones son por indicación, además porque el socio no cumple con los requisitos, o el tiempo especifico pactado.
.
SIN COMENTARIOS
ninguna
.
.
Ninguno
posibilidad de habilitar que se puedan recibir documentos por los medios con los que contamos en sucursales, y asi mejorar la experiencia de servicion en los socios.
TODO BIEN  GRACIAS
un poco mas de agilidad para identificar en sistema reportes de solicitudes
EL CURSO CONTIENE LA INFORMACIÓN NECESARIA
.
ninguno
Con la capacitacion que nos presentaron al  momento todo esta mejorando
Duda: Se podra realizar el mantenimiento de la solicitud cuando el socio desea cancelar la solicitud una vez estando en la etapa de desembolso por cuestiones de cambio de condiciones por parte del socio ?
  NINGUNA
.
EXCELENTE INFORMACION
que procede en caso de cancelar una solicitud por error de producto o porque el socio desee cambiar el medio para formalizar es decir sucurarsal y en app
ok
todo bien
excelente
Contiene muchos archivos para descargar
interesante informacion
.
Es suficiente la informacion
.
ninguna
LA INFORMACION FUE CLARA Y ENTENDIBLE
curso finalizado
Todo esta completo
buen curso
SIN COMENTARIOS
se cumplio con el objetivo
esta todo muy claro felicidades por esta nueva estetegia
Excelente curso
INFORMACION INTERESANTE
Esta bien. Gracias
Filtros en crm para revisar las solicitudes en este supuesto y asi poder dar seguimiento , contanto con el numero de socio, nombre, y analista que lo realizo
Muy buena informacion ydistribucion del contenido
ok
ESPERO QUE CON ESTE CAMBIO NOS AYUDE EN TENER MAYOR CONTACTO CON EL SOCIO, POR QUE MUCHAS VECES LE MARCAMOS Y MUY POOCOS ACUDEN EL DIA PACTADO, POR QUE LE RESTAN IMPORTACIA.
ninguna, gracias
NINGUNO
ES MUY IMPORTANTE  CONSIDERA ESTAS INDICACIONES
INFORMACION IMPORTANTE
MAS QUE NADA EL APLICAR LOS CONOCIMIENTOS OBTENIDOS PARA EVITAR EN GRAN MEDIDA LA CANCELACIÓN DE ALGUNA SOLICITUD
.
muy claro
De acuerdo a las estadisticas se refuerza la importancia de la colocacion de creditos
TODO ENTENDIBLE GRACIAS
POR EL MOMENTO ESTA TODO BIEN
CONSIDERO QUE POR EL MOMENTO SE TIENE LA INFORMACION NECESARIA. BUENA INICIATIVA LA CUAL NOS APOYARA  A LOGRAR LOS PBJETIVOS DEL 2025
..
NINGUNA
Todo esta claro, MUCHAS GRACIAS
todo bien
ninguno
Hasta el momento la información es clara y suficiente.
..
ninguno
SATISFACTORIO
consideró que tengo las guías para hacer el proceso de manera adecuada y dar menor servicio al socio
CREO SERIA MUY BUENO CONTAR CON UNA RED WIFI CON MEJOR CONECTIVIDAD, PARA PODER AGILIZAR LOS PROCESOS DE SOLICITUDES DE CREDITOS
excelente
buen curso
ninguno
ESTA ESTRATEGIA SERVIRA PARA MEJORAR EL SERVICIO Y AUMENTAR LA COLOCACION DE CREDITOS
.
.
.
BIEN
De momento ninguno, esta clara la informacion
todo bien
.
ES SUFICIENTE SE LOGRARA EL OBJETIVO, SALUDOS.
tema importante
ninguna
HASTA AHORITA TODO BIEN.
POR EL MOMENTO NO ES NECESARIO ESO PUDIERA SURGIR EN EL CAMINO...
SIN COMENTARIOS
Estas nuevas estrategias contribuyen a mejorar la colocación de sucursal un indicador muy importante
.
buen curso
Las estadísticas por sucursal para saber en que trabajar
GRACIAS
aun ninguna...en cuanto ya iniciemos se va requiriendo de dudas y solicitando apoyo.
El curso contiene la información necesaria para arrancar con el programa, de momento no requiero información adicional
de momento ninguna
gracias
fue corto
El socio podra enviar informacion por medios electronicos?
  ENTENDIDO
muchas gracias
sin comentario
Todo estuvo bien, Gracias.
Tengo la siguiente duda: Qué pasa cuando se necesita cambiar de finalidad, si ya no la vamos a poder cancelar en sucursal, habrá alguna forma para que desde mesa de ayuda la cancelen?
  .
Al momento todo esta correcto
NINGUNA
por el momento ninguno
Existe al día de hoy una gran área de oportunidad para validar los teléfonos en sucursal, por servicio y practicidad es necesario cuenten con un equipo móvil para validar los números de teléfono de los socios, incluso para apoyar a los socios con consultas sobre las apps. Me parece un acierto que se retome el tema del porque se cancelan las solicitudes, me parece también que es necesario se identifiquen puntualmente por plaza las principales incidencias que se dan en lo particular y poder trabajarlas. También necesario prepara al personal porque el servicio es bien importante, en este mundo de tecnologia el diferenciador será el trato con el socio.
GRACIAS
na
ok
por el momento no tengo alguna
De momento no, Gracias
ME QUEDA CLARA LA INFORMACION PARA PODER DESEMPEÑAR MEJOR MI TRABAJO
Todo bien
Gracias
si
:)
SIN OBSERVACIONES, LA INFORMACION ES CLARA Y DE FACIL COMPRENSION
EXCELENTE CURSO
TODO MUY BIEN
.
KJ
.
LA MAYORIA DE LAS SOLICITUDES QUE SE CANCELAN, EL SOCIO REGRESA A LEVANTAR SU SOLICITUD.
cubre las necesidades.
Revisar cuando sea viable la solicitud y cuando no, no se le de la opción
CAPACITACION PRESENCIAL NO SOLO TOMADA EN LINEA OBSERVANDO LAS DIAPOSITIVAS, MOSTRANDO LAS EXPERIENCIAS QUE SE REQUIEREN OBTENER.
GRACIAS
Con el fin de lograr la disminución del número de cancelaciones en solicitudes
Por el momento ninguno
no tengo comentarios
ninguna
FUE MUY BUENA LA EXTRATEGIA POR QUE BA SER MAS PERSONALISADA LA ATENCION Y EL SEGUIMIENTO A LOS TRAMITES
***
  CURSO DE MUCHO INTERES
.
todo bien.
SIN COMENTARIOS
considero que las implementaciones de estos cambios seran buenos
todo bien
GRACIAS
POR EL MOMENTO NINGUNA, HASTA LA OPERACION SE VERA SI HAY DUDAS
lo que pasa es que uno mismo provoco todo esto, ya que por indicaciones de arriba decian que a todo ingreso se le debia de generar una asesoria aun cuando el socio solo queria ahorrar y esto genero falsas expectativas de credito cuando realmente el socio no queria credito
guias de apoyo
MUY BUENO
x
sin comentarios
OK
INFORMACION GENERAL SOBRE LAS FINALIDADES DE LOS CREDITOS.
TODO BIEN
cundo por la distancia el socio tenga la necesidad de realizar su solicitud por la aplicacion como poder realizar el proceso de cancelacion ya que al tener activa la solicitud en sucursal no le va a permitir iniciar una nueva ?
  que toma en consideración el sistema para evaluar dichas solicitudes
Buen curso
gracias
considero que el materia proporcionado esta completo respecto al cambio de solicitudes canceladas
bien
TODO BIEN
bien
n/a
.
material suficiente para entender el tema
.
Ninguna
MAS COMUNICACION
.
Información novedosa e interesante
ok
Que se homologue los requisitos y documentación a presentar a los socios informales con ingresos variables.
OK
ninguna
LA INFORMACION ME AYUDARA A REALIZAR DE MEJOR FORMA MI TRABAJO
sin comentarios
no por el momento
.
una resumen
bien
.
EN BUENAHORA ESTAS NUEVAS MEJORAS AYUDARAN A BRINDAR UN MEJOR SERVICIO A NUESTROS SOCIOS
LA INFORMACION ES DE SUMA IMPORTANCIA
"PODRIAN CONSIDERAR PONER UN FILTRO PARA SOLICITUDES EN INVESTIGACION DE SUCURSAL
TAMBIEN SOLICITUDES CON BURO DENEGADO.
ALGUN APARTADO PARA PONER OBSERVACIONES DEL SEGUIMIENTO QUE SE LE ESTA PROPORCIONANDO A LA SOLICITUD, ACTUALMENTE LO HAGO EN NOTAS PERO NO ES MUY VISIBLE, PARA ALGUNA CONSULTA.
CONSIDERO LA TECNOLOGIA NOS PROPONE MEJORAS MUY BUENAS PARA SER EFICIENTES Y EFICACES AL MOMENTO DE LA ATENCION CON EL SOCIO PARA MEJORAR LOS TIEMPOS, SIN EMBARGO SE NOS INDICA CONTINUAR CON MECANISMOS DEL PASADO QUE SON REPROCESOS ACTUALMENTE."
sin comentarios
....
Con esta nueva metodología, se puede decir que CPM busca un acercamiento con nuestros socios aun mas con la finalidad de poder cubrir el rubro del crédito en general .
del total de solicitudes de canceladas por falta de requisitos, identificar el motivo por el cual el socio no puede presentar los requisitos solicitado (documentación)
sc
ninguno
Ok
NINGUNA
OK
Sin comentarios
.
la informacion esta muy completa
.
NINGUNA
ok
todo bien
ninguno
Gracias
Ninguno
informacion recibida.
sin comentario
excelente
QUE LO QUE SALGA APROBADO POR BURO SE ENTREGE DE MANERA INMEDIATA.
excelente contenido
CREO QUE ES IMPORTANTE SEGUIR REFORZANDO ESTOS TEMAS CADA CIERTO TIEMPO
.
.
ninguno
La informacion proporcionada fue de gran ayuda no se requiere de algo adicional. Gracias
LOS ARCHIVOS EN PDF SERÁN DE IMPORTANCIA PARA VALIDAR ALGUNA DUDA QUE SURJA
ES UNA INICITIVA MUY BUENA POR QUE SE PUEDE RESCATAR CREDITOS
SIN COMENTARIOS
sin comentarios
.
todo claro
TODO BIEN
"ESTÁ MUY COMPLETO Y ENTENDIBLE.
GRACIAS"
informacion importante
.
todo bien
.
.
"TODO ESTÁ COMPLETO Y ESPECIFICADO.
GRACIAS"
muy importante
BIEN
el curso me ayudo a conocer el proceso , pienso en el transcurso del proceso saldran dudas
..
.
por el momento no hay nada que requiera adicional
NO CAMBIARIA NADA EL PROCESO QUE SE ESTARA  REALIZANDO SE ME HACE CORRECTO.
no ayuda a conocer mas informacion de los procesos internos
.
Con la informacion que nos han estado compartiendo me ha sido de facil comprension, por lo que continuar de esa manera
Sin comentarios
--------
  BIEN
TODO CLARO Y PRECISO
EL MATERIAL ES MUY COMPLETO Y UTIL
bien
GRACIAS
.
EXCELENTE INFORMACION
sin comentarios
EXCELENTE
Vamos por mucho mas  todos los cambios y mejoras serán muy buenos siempre y cuando quienes lo vendan y ofrezcan lo hagan con el compromiso total ponerse la playera de CPM
LA INFORMACION VISTA A LO LARGO DE ETE CURSO NOS PERMITE ESTAR EN LA FRECUECIA ADECUEDA PARA BRINDAR UN BUEN SERVICIO
sin comentarios
Sin comentarios
gracias!
  todo esta muy bien con las guias
BIEN
.
bueno
capacitaciones
...
EXCELENTE CURSO
.
CON EL MATERIAL PROPORCIONADO FUE DE MUCHA AYUDA
NINGUNO
NINGUNA
.
que la actualizacion de datos de manera general se quede guardada y no tener que hacerla a cada tramite
.
buen curso
Al momento ninguno, sobre la marcha si salen dudas con el proceso espero aclararlas con mi equipo de trabajo.
LA INFORMACION DE DONDE SE PUEDEN EXTERNAR LAS DUDAS QUE SURGAN.
NINGUNA
ESTOY COMENZANDO EN EL NUEVO PUESTO COMO ANALISTA, PERO PONDRE TODO DE MI PARTE PARA LA COLOCACION DE CREDITOS
EXCELENTE
informacion necesaria
ninguno
.
dar agilidad y respuesta mayor a las solicitudes de credito para que sean favorables
información clara y oportuna
TODO BIEN
d
excelenete
.
OTORGANDO UNA BUENA ATENCION Y BRINDANDO MEJOR SERVICIO CUMPLIREMOS LA META PARA NO TENER GRAN CANTIDAD DE CREDITOS CANCELADOS
NA
gracias
n
SE LOGRO CON EL OBJETIVO DEL CURSO
.
no
EXCELENTE
OK
ninguna por el momento
SIN COMENTARIOS
8
a
buen curso
HASTA COMEZAR A OPERAR CON ESTA IMPLEMENTACIÓN SABRIA QUE MAS REQUIERO CONOCER
POR EL MOMENTO NO CUENTA CON NINGUNA DUDA SOBRE LA MARCHA EXPONDRE MI DUDA A MI JEFE INMEDIATO PARA ESCALARLA
Todo bien.
.
cuando se dio la capacitacion yo estaba de vacaciones , solo me informaron la idea general de este cambio , pero de momento no se me ha complicado
ninguna
bueno
tal vez ya lo tienen planeado pero seria util contar con la base de datos o la manera mas agil de hacer los filtros en crm para obetener un listado de estos creditos
NINGUNO
BUEN MATERIAL DE APOYO
NA
LOS VIDEOS HACEN REFLEXIONAR
informacion de gran ayuda en la actualizacion de nuestro sistema para mejor la calidad en el servicio y seguimiento de nuestros socios
muy interesante el tema para retomar muchas medidas y tencion al socio
NINGUNO
excelente
NINGUNA LA INFORMACION ESTA MUY CLARA
.
muy clara la información que se visualizo.
s/c
en este momento ninguno
.
la informacion recibida por el momento es muy util y nos ayudara en disminuir el numero de cancelaciones en solicitudes, e incrementar los numeros de solicitudes aprobadas, dandole seguimiento hasta concretar la formalizacion del credito
bien
conocer como funciona la metodologia del credinamico y credito personal y vigencia
.
LA PRACTICA
TODO  MUY BIEN
NINGUNA POR EL MOMENTO
:)
.
ACTUALIZAR LAS GUIAS DE LOS PROCESOS O BIEN ESPECIFICAR QUE HACER CUANDO ALGUNA SOLICITUD DE CREDITO QUE EL RESULTADO SEA DENEGADO YA QUE EN EL CURSO SE ESPECIFICA QUE NO SE PUEDEN CANCELAR. ACLARAR QUE HACER CON ESAS SOLICITUD QUE NO SON VIABLES CUANDO TIENEN SALDOS VENCIDOS Y NO SE PUEDA CONTINUAR
ninguno
GRACIAS
.
BIUEN CONTENIDO
ninguno
.
SIN COMENTARIOS
excelente
EXCELENTE
todo bien
sin comentarios
EL MATERIAL EXPLICA EL CONTEXTO Y ALCANCE DEL PROYECTO.
ninguna
NINGUNA TODO BIEN
.
TODA LA INFORMACION EXCELENTE
NINGUNA
sobre el funcionamiento o el impacto qaue tendra este promagra
sc
.
Únicamente dar seguimiento a la estrategia para poder medir su efectiva e impacto para realizar los ajustes que se requieran.
DESDE YA, PONERLO EN PRACTICA.
todo bien
SE PODRA REALIZAR LA CANCELACION CUANDO ES ERROR DE CAPTURA O CAMBIO DE CONDICIONES
.....
MUY BIEN
Ninguno
buen curso.
.
TODO ESTA PERFECYTO, TENEMOS LA INFORMACION  REQUERIDA
ninguno
El servicio al socio es fundamental, pero al turnar que el gerente es quien dé tema al personal, para mi NO es lo correcto debe ser el área especializada Gerencia de crédito pues solo así se lograra concientizar al equipo de trabajo de la importancia de la satisfacción y atención al socio, en muchas ocasiones el mismo gerente de sucursal influye en prácticas no deseadas o consideraciones a su criterio que no son las adecuadas, o no se involucra en que se esté cumpliendo con las políticas y procesos de crédito.
=)
.
ES UNA EXCELNETE INICIATIVA
BUENOS VIDEOS
ninguna
EXCELENTE INFORMACION
EXCELENTE CURSO
DE MOMENTO TODO BIEN
VAMOS !!!
  se cuentan con las herramientas necesarias para disminuir el numero de solicitudes canceladas
SIENTO QUE POR EL MOMENTO ME SIENTO CAPAZ DE DESARROLLAR MIS ACTIVIDADES DE LA MEJOR MANERA, ME GUSTARIA QUE PUDIERAN DARNOS UNA RETROALIMENTACION MAS CLARA DE NORMATIVA YA QUE CON MUCHOS CAMBIOS SE NOS OLVIDAN ALGUNOS CRITERIOS DE NORMATIVA
buen curso
buena idea
GRACIAS
.
MUY BIEN
ok
LA INFORMACION BRINDADA DURANTE EL CURSO FUE CLARA Y PRECISA POR LO QUE CONSIDERO QUE ES OPORTUNA PARA EL CORRECTO APLICAMIENTO DEL MISMO.
todo ok
EXCELENTE CURSO QUE NOS PERMITE ESTAR MEJOR INFORMADOS Y BRINDAR UNA EXCELENTE ASESORIA.
GRACIAS
MAYOR RAPIDEZ EN LA RESOLUCION DE CREDITOS PREFERENTEMENTE AUTOMOTRICES E HIPOTECARIOS
Gracias
interesante
NA
EXCELENTES AVANCES PARA LA MEJORA DE NUESTRO SERVICIO
POR LO PRONTO SOLO QUE SE ME MANTENGA INFORMADO DE CUALQUIER ACTUALIZACIO.
que de alguna manera se pueda estar en contacto directo con los analistas que tienen las solicitudes, para dar seguimiento a las solicitudes.
.
Que toda la información que se actualice nos lo compartan al momento.
Muy clara la informacion.
EXCELENTE
ok
.
excelente curso
Todo bien
INFORMACION IMPORTANTE
DAR TODA LA INFORMACION COMPLETA AL SOCIO DEL PROCESO CREDITICIO
METODOLOGIA DE CONSUMO
sin comentarios
CONSIDERO
NINGUNO
ES UNA BUENA ESTRATEIA
.
Sin comentarios
NA
EXCELENTE INFORMACION
MUY INFORMATIVO, QUE SE VENGAN NUEVOS CAMBIOS
sin comentarios
CONSIDERO QUE HASTA EL MOMENTO LA INFORMACION A SIDO CLARA Y PRECISA POR LO QUE POR EL MOMENTO NO EXISTEN DUDAS
ninguna
GUIA PRACTICA
gracias
OK
excelente
muy bien
bien
Me gusto el curso.
na
Todo Bien
.
capacitaciones presenciales
NINGUNO.
SIN COMENTARIOS
QUE TENGA MAS EJEMPLOS Y SEA MAS DIDACTICO
TODA LA INFORMACION RECIBIDA DURANTE EL CURSO Y LAS CAPACITACIONES ME HAN AYUDADO A INDENTIFIAR LOS NUEVOS PROCESOS A IMPLEMENTAR EN EL NUEVO PROGRAMA DE SMART CORE.
NINGUNA
EXELENTE
.
ninguna, es clara y precisa .
Sin comentarios
NINGUNA
de momento todo bien
bien
SERIA BUENO ENVIAR MENSAJE POR WHATSAAP A NUESTROS SOCIOS COMO RECORDARTORIO DE QUE CUENTAN CON UNA PRECALIFICACION ACTUAL DE ALGUNA MANERA ESO AYUDARIA MUCHO PARA PODER INCREMENTAR LA COLOCACION DE CREDITO ASI COMO DARLE EL SEGUIMIENTO A SU SOLICITUD .
quiza un poco mas de informacion detallada
es buena le dra tiempo y seguimiento para que los socios continuen con el tramite de credito
no aplica
na
EL CURSO ESTA MUY COMPLETO Y FACIL DE COMPRENDER Y APRENDER
sin comentarios
Recibir capacitacion en todas sus etapas sobre el proceso del credito en CRM
todo bien
todo claro
.
De momento ninguna.
nada, gracias
todo bien
INFORMACION MUY IMPORTANTE PARA TODO EL PERSONAL
Este curso es de mucha utilidad para nuestro trabajo
que se esté actualizando la información de acuerdo a lo que se vaya implementando
Desde mi puesto, listados que me permitan ir evaluando el avance en las sucursales, pero estos los puedo generar desde crm
.
considero que es importante tener esta informacion en un rsumen general que este al alcance de todos los analistas,
Ninguna
"De manera eventual estadística sobre el comportamiento o efectividad que se tiene teniendo ya los cambios implementados.
Ejemplo: formalizaciones sobre socios a los cuales ya se le haya envido mensaje de formalización y tiempo de atención."
ESTA MUY COMPLETO
NINGUNA
ninguna
ninguna
MUCHAS GRACIAS!!
  Ninguna
nada
existen algunos temirnos en relacion a proyectos que no entendemo s
Todo está bien
NINGUNO
.
SE TOMO TALLER
LAS RECOMENDACIONE PARA NO DISMINUIR LAS CANCELACIONES Y PODER CONCLUIR ESTE TIPO DE TRAMITES PARA EL INCREMENTO DE INDICADOR DE COLOCACION.
SOBRE LA MARCHA DE APRENDE Y SE DETECTAN LAS NECESIDADES
NINGUNA  CREO QUE CON MI TRABAJO BIEN REALIZADO ESTE PROGRAMA SERA UN EXITO, ESPERO TODOS LO PODAMOS COMPRENDER
gracias cpm
no
ningun comentario
gracias
.
mayor información
Ninguna
sin comentarios.
*
  .
todo bien
Ninguna
todo quedo claro
Todo bien
Ninguna.
informacion interesante
.
Es muy importante disminuir la cancelacion de solicitudes aprobadas por la metodoligia
con la capacitación y la información que nos dan es sufuciente, gracias.
todo bien
.
La información es clara y comprensible.
.
Se requiere solo del acompañamiento durante la salida en vivo
Excelemte informacion
ok
Considero que tenemos la herramientas y conocimientos necesario para disminuir en gran medida las cancelaciones de solicitudes aprobadas por la metodología
todo bien
EXCELENTE
.
gracias
Gracias
gracias
todo bien
por el momento ninguna
Ningruna, gracias
Es un material muy bueno que nos ayuda a mejorar el servicio y la experiencia dentro de CPM
.
***
  Todo muy bien
Gracias
SABER EL PORCENTAJE DE LAS CAUSAS EXTERNAS Q HACEN Q NUESTROS SOCIOS CANCELEN SUS SOLICITUDES
.
UN REFORZAMIENTO CON RESPECTO A LOS RESULTADOS EN BURO DE CREDITO, PARA DARLE APOYO AL SOCIO, CUANDO SE TRATE DE CREDITOS LIQUIDADOS CON ANTERIORIDAD DE MANERA EXTERNA
todo bien!
  pues yo pienso que con el avance iremos viendo las necesidades para el conocimiento
CONSIDERO COMPLETA LA INFORMACION RECIBIDA
buenos cambios
información precisa de cuales solicitudes son por iniciativa del socio y cuales son precalificaciones al momento de que se ingresa el socio
flexibilizar los requisitos
Se complementa con las preguntas frecuentes para tenerlas  de apoyo mas practico, las cuales ya están cargadas también.
PUES SIENTO QUE EL CURSO VIENE MUY COMPLETO Y ENTENDIDO PARA LLEVAR ACABO LA DISMINUCION DE SOLICITUDES ASI COMO GRACIAS POR TODO EL APOYO PARA QUE ESTO SE LLEVE A CABO EN CUANTO A LOS ENVIOS DE MENSAJES
NINGUNA
gracias
GRACIAS!
  MUY UTILES ESTE TIPO DE CURSO
.
.
Sobre la practica puedo ver que más necesito, sé que contaré con el apoyo necesario
x
ninguna
buen curso
..
ok
de momento no, tal vez cuando salga en vivo
Reportes de avances de como va la estrategia y que hay que redireccionar, mantenernos comunicados de los resultados
el curso es muy claro y entendible
ninguna
SUGERENCIA. PODER VISUALIZAR LE PRESENTACION GRABADA. PARA PODER ESCUCHA LOS COMENTARIOS DE NUEVO, YA QUE A VECES NOS INTERRUMPEN EN SUCURSAL CUANDO ESTAMOS EN LOS CURSOS.
ok
no
TODO BIEN
excelente curso
