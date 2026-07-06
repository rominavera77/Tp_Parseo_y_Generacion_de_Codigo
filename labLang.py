import ply.lex as lex
import ply.yacc as yacc

# ==============================================================
# Analizador Léxico y Sintáctico para el Lenguaje de Laboratorio
# ==============================================================

class MiCompilador:
    # palabras reservadas del lenguaje
    reserved = {
        'Experiment': 'EXPERIMENT',
        'Measure': 'MEASURE',
        'at': 'AT',
        'seconds': 'SECONDS',
        'Record': 'RECORD',
        'Analyze': 'ANALYZE',
        'with': 'WITH',
        'if': 'IF',
    }
    # lista de tokens, incluyendo palabras reservadas
    tokens = [
        'STRING', 'INT', 'FLOAT',
        'LBRACE', 'RBRACE',
        'LPAREN', 'RPAREN',
        'SEMI', 'EQUALS',
        'COMP_EQ', 'LT', 'GT',
        'ID'
    ] + list(reserved.values())

    # expresiones regulares para tokens simples
    t_LBRACE  = r'\{'
    t_RBRACE  = r'\}'
    t_LPAREN  = r'\('
    t_RPAREN  = r'\)'
    t_SEMI    = r';'
    t_COMP_EQ = r'=='
    t_EQUALS  = r'='
    t_LT      = r'<'
    t_GT      = r'>'

    # funciones para tokens con acciones semánticas

    # identificadores y palabras reservadas
    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value, 'ID')
        return t

    # números enteros y flotantes
    def t_FLOAT(self, t):
        r'\d+\.\d+'
        t.value = float(t.value)
        return t

    def t_INT(self, t):
        r'\d+'
        t.value = int(t.value)
        return t
    
    # cadenas de texto
    def t_STRING(self, t):
        r'\"([^\\\n]|(\\.))*?\"'
        t.value = t.value[1:-1]
        return t
    
    # ignorar espacios y tabulaciones
    t_ignore = ' \t\r\n'

    # manejo de errores léxicos
    def t_error(self, t):
        print(f"   ⚠️ ERROR LÉXICO detectado: Carácter ilegal '{t.value[0]}' en la posición {t.lexpos}")
        t.lexer.skip(1)

    # ----------------------------------------------------------------------------
    # PARSER (Sintáctico y Semántico)
        # Aquí se definen las reglas gramaticales del lenguaje y
        # se implementan las acciones semánticas correspondientes para cada regla.
    # ----------------------------------------------------------------------------

    def __init__(self):
        # La tabla de símbolos es una estructura fundamental en un compilador, 
        # ya que guarda información sobre los identificadores utilizados durante la compilación.
        self.tabla_simbolos = {}
        self.error_sintactico_detectado = False

    def p_experiment(self, t):
        '''experiment : EXPERIMENT STRING LBRACE steps RBRACE'''
        if not self.error_sintactico_detectado:
            print(f"   ✅ [Sintaxis OK] Experimento '{t[2]}' finalizado correctamente.")

    def p_steps(self, t):
        '''steps : step
                 | steps step'''
        # No hace ninguna acción porque solamente agrupa instrucciones
        pass

    def p_step_if(self, t):
        '''step : IF LPAREN condition RPAREN LBRACE steps RBRACE'''
        if t[3]:
            print("   ℹ️ [Flujo Semántico]: Condición Cumplida (True). Bloque IF ejecutado.")
        else:
            print("   ℹ️ [Flujo Semántico]: Condición Falsa (False). Bloque IF ignorado.")

    def p_condition(self, t):
        '''condition : ID COMP_EQ value
                     | ID LT value
                     | ID GT value'''
        var_name = t[1]
        operador = t[2]
        valor_comparar = t[3]

        if var_name in self.tabla_simbolos and self.tabla_simbolos[var_name] is not None:
            valor_actual = self.tabla_simbolos[var_name]
            if operador == '==':   t[0] = (valor_actual == valor_comparar)
            elif operador == '<':  t[0] = (valor_actual < valor_comparar)
            elif operador == '>':  t[0] = (valor_actual > valor_comparar)
        else:
            print(f"   ⚠️ ERROR SEMÁNTICO: La variable '{var_name}' no existe en la Tabla de Símbolos.")
            t[0] = False

    def p_step_measure(self, t):
        '''step : MEASURE STRING AT INT SECONDS SEMI'''
        variable = t[2]
        tiempo = t[4]
        print(f"   -> Midiendo {variable}...")
        if variable not in self.tabla_simbolos:
            self.tabla_simbolos[variable] = None

    def p_step_record(self, t):
        '''step : RECORD STRING EQUALS value SEMI'''
        variable = t[2]
        valor = t[4]
        self.tabla_simbolos[variable] = valor
        print(f"   -> Registrando {variable} = {valor}")

    def p_step_analyze(self, t):
        '''step : ANALYZE STRING WITH STRING SEMI'''
        variable = t[2]
        metodo = t[4]
        if variable in self.tabla_simbolos:
            print(f"   -> Analizando {variable} con {metodo}. (Valor en TS: {self.tabla_simbolos[variable]})")
        else:
            print(f"   ⚠️ ERROR SEMÁNTICO: No se puede analizar '{variable}' porque no ha sido medida/registrada.")

    def p_value(self, t):
        '''value : INT
                 | FLOAT'''
        t[0] = t[1]

    def p_error(self, t):
        self.error_sintactico_detectado = True
        if t:
            print(f"   ⚠️ ERROR SINTÁCTICO detectado en el token '{t.value}' (Línea {t.lineno})")
        else:
            print("   ⚠️ ERROR SINTÁCTICO detectado: Fin de archivo inesperado.")

# # ------------------------------------------
# # FUNCIÓN PARA GUARDAR EN TXT
# # ------------------------------------------
#     def guardar_records_txt(self, nombre_archivo):
#         try:
#             with open(nombre_archivo, "w", encoding="utf-8") as archivo:
#                 # Escribimos un encabezado estético
#                 archivo.write("=" * 45 + "\n")
#                 archivo.write("       RECORDS GUARDADOS POR EL COMPILADOR\n")
#                 archivo.write("=" * 45 + "\n")
#                 archivo.write(f"{'VARIABLE':<15}{'TIPO':<15}{'VALOR':<15}\n")
#                 archivo.write("-" * 45 + "\n")
                
#                 # Recorremos la tabla de símbolos y formateamos las columnas
#                 for var, datos in self.tabla_simbolos.items():
#                     archivo.write(f"{var:<15}{datos['tipo']:<15}{str(datos['valor']):<15}\n")
                
#                 archivo.write("=" * 45 + "\n")
#             print(f"\n💾 [Archivo]: Todos los records han sido exportados a '{nombre_archivo}'")
#         except IOError as e:
#             print(f"   ⚠️ Error al intentar escribir el archivo TXT: {e}")

#     # Método auxiliar para probar el compilador
#     def analizar(self, codigo):
#         self.error_sintactico_detectado = False
#         self.tabla_simbolos.clear()
#         self.parser.parse(codigo)

# ==========================================
# CÓDIGOS DE PRUEBA DEL BANCO DE ENSAYOS
# ==========================================

# Prueba 1: Diseñada para forzar un error Léxico (Usa caracteres prohibidos como $, % o #)
prueba_lexico = '''
Experiment "Prueba Lexica" {
    Record "temperatura" = 25$;
    Record "humedad" = 60%;
}
'''

# Prueba 2: Diseñada para forzar un error Sintáctico (Falta un punto y coma y unos paréntesis del IF)
prueba_sintaxis = '''
Experiment "Prueba Sintactica" {
    Record "temperatura" = 25

    if temperatura == 25 {
        Record "alerta" = 1;
    }
}
'''

# Prueba 3: Sintaxis y caracteres perfectos, pero evalúa errores semánticos (Usa variables no definidas)
prueba_semantica = '''
Experiment "Prueba Semantica" {
    Record "temperatura" = 30;

    if (presion == 1.2) {
        Analyze "volumen" with "GasesIdeales";
    }
}
'''

# ==========================================
# FUNCIÓN EJECUTORA DEL BANCO DE PRUEBAS
# ==========================================
def ejecutar_test(nombre, codigo_fuente):
    print("\n" + "="*60)
    print(f" EJECUTANDO: {nombre}")
    print("="*60)
    print("Código fuente evaluado:")
    print(codigo_fuente.strip())
    print("-"*60 + "\nResultados del Análisis:")

    # Creamos un compilador nuevo y limpio para cada test
    instancia = MiCompilador()
    lexer = lex.lex(module=instancia)
    parser = yacc.yacc(module=instancia, debug=False, write_tables=False)

    parser.parse(codigo_fuente)

# Lanzamos todo el set de pruebas
ejecutar_test("TEST 1: CONTROL DE ERRORES LÉXICOS", prueba_lexico)
ejecutar_test("TEST 2: CONTROL DE ERRORES SINTÁCTICOS", prueba_sintaxis)
ejecutar_test("TEST 3: CONTROL DE ERRORES SEMÁNTICOS", prueba_semantica)
