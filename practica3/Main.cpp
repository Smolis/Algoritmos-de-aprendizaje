#include <iostream>
#include <string>
#include <fstream>
#include <math.h>
#include <vector>

using namespace std;

const int MAX = 100;
const long double PI = 3.141592653589793238;
const long double E = 2.718281828;
double datos[MAX][MAX];


/*
Carga el fichero Iris2Clases.txt y según la clase a la que pertenezca la muestra, Iris-setosa o Iris-versicolor, 
le asigna el valor 0 o 1 respectivamente al guardarlo en la matriz
A su vez lleva la cuenta del total de filas y de columnas para su posterior uso
*/
bool cargarFichero(int &filas, int &columnas) {
	bool encontrado = true;
	ifstream archivo;
	string nombre = "Iris2Clases.txt";
	string fila = "";
	archivo.open(nombre);

	if (!archivo.is_open()) {
		cout << "No se puede encontrar el archivo" << endl;
		encontrado = false;
	}
	else {
		int f = 0;
		int c = 0;
		int numC = 0;
		bool fin = false;
		while (archivo >> fila) {
			int i = 0;
			string cadena = "";
			while (i < fila.length() && !fin) {
				if (fila[i] == 'I') {
					int b = 0;
					string tipo = "";
					while (b + i < fila.length()) {
						tipo += fila[b + i];
						b++;
					}
					if (tipo == "Iris-setosa") {
						datos[f][c] = 0;
					}
					else {
						datos[f][c] = 1;
					}
						
					fin = true;
				}
				else {
					if (fila[i] != ',') {
						cadena += fila[i];
						
						
					}
					else {
						double num = atof(cadena.c_str());
						datos[f][c] = num;
						c++;
						cadena = "";
						if (f == 0) {
							numC++;
						}
					}
				}
				i++;
			}
			c = 0;
			f++;
			fin = false;
		}
		filas = f;
		columnas = numC;
		

	}
	
	return encontrado;
	
}

double calcularM(int filas, int columna, int columnas, double tipo) {
	double m = 0;
	double cont = 0;
	for (int i = 0; i < filas; i++) {
		double t = datos[i][columnas];
		if (datos[i][columnas] == tipo) {
			m += datos[i][columna];
			cont++;
		}
	}

	m = m / cont;
	return m;
}

vector<vector<double>> multiplicarVectores(vector<double> fila, vector<double> fila2) {
	vector<vector<double>> matriz(0);
	for (int i = 0; i < fila.size(); i++) {
		vector<double> res(0);
		for (int j = 0; j < fila2.size(); j++) {
			res.push_back(fila[i] * fila2[j]);
		}
		matriz.push_back(res);
	}

	return matriz;
}

vector<vector<double>> dividirMatriz(vector<vector<double>> matrix, int filas) {
	vector<vector<double>> resultado(0);
	for (int i = 0; i < matrix.size(); i++) {
		vector<double> fila(0);
		for (int j = 0; j < matrix[i].size(); j++) {
			double a = matrix[i][j];
			double b = a/filas;
			fila.push_back(b);
		}
		resultado.push_back(fila);
	}
	return resultado;
}

vector<vector<double>> sumarMatrices(vector<vector<double>> matrix, vector<vector<double>> m) {
	vector<vector<double>> resultado(0);
	for (int i = 0; i < matrix.size(); i++) {
		vector<double> fila(0);
		for (int j = 0; j < matrix[i].size(); j++) {
			double a = matrix[i][j];
			double b = m[i][j];
			fila.push_back(a+b);
		}
		resultado.push_back(fila);
	}
	return resultado;

}

vector<vector<double>> calcularC(int filas, int columnas, int tipo, vector<double>vm) {
	vector<vector<double>> matrix(0);
	int numF = 0;
	for (int i = 0; i < filas; i++) {
		vector<double> fila(0);
		if (datos[i][columnas] == tipo) {
			for (int j = 0; j < columnas; j++) {
				fila.push_back(datos[i][j]-vm[j]);
			}
			vector<vector<double>> m = multiplicarVectores(fila, fila);
			if (numF == 0) {
				matrix = m;
			}
			else {
				matrix = sumarMatrices(matrix, m);
			}
			numF++;
		}
	}
	matrix = dividirMatriz(matrix, numF);
	return matrix;
}
vector<vector <double>> calcularAdj(vector<vector <double>> matriz, int fila, int col) {
	vector<vector<double>> adj(0);
	for (int f = 0; f < matriz.size(); f++) {
		if (f != fila) {
			vector<double> fila(0);
			for (int c = 0; c < matriz.size(); c++) {
				if (c != col) {
					fila.push_back(matriz[f][c]);
				}
			}
			adj.push_back(fila);
		}
	}
	return adj;
}

double detAdj(vector<vector <double>> matriz) {
	double p = (matriz[0][0] * matriz[1][1] * matriz[2][2]) + (matriz[0][1] * matriz[1][2] * matriz[2][0]) + (matriz[1][0] * matriz[2][1] * matriz[0][2]);
	double s = (matriz[0][2] * matriz[1][1] * matriz[2][0]) + (matriz[2][1] * matriz[1][2] * matriz[0][0]) + (matriz[1][0] * matriz[0][1] * matriz[2][2]);
	double det = p - s;
	return det;
}
double determinante(vector<vector<double>> matriz) {
	double d = 0;
	for (int i = 0; i < matriz.size(); i++) {
		vector<vector<double>> adj = calcularAdj(matriz, i, 0);
		if (i % 2 == 0) {
			d += matriz[i][0] * detAdj(adj);
		}
		else {
			d -= matriz[i][0] * detAdj(adj);
		}
	}
	return d;
}

vector<vector<double>> matrizTraspuesta(vector<vector<double>> matriz) {
	vector<vector<double>> traspuesta(0);
	
	for (int i = 0; i < matriz.size(); i++) {
		vector<double> fila(0);
		for (int j = 0; j < matriz.size(); j++) {
			fila.push_back(matriz[j][i]);
		}
		traspuesta.push_back(fila);
	}

	return traspuesta;
}
vector<vector<double>> inversa(vector<vector<double>> matriz) {
	double det = determinante(matriz);
	vector<vector<double>> traspuesta(0);
	vector<vector<double>> adj(0);
	traspuesta = matrizTraspuesta(matriz);

	double d = -1;
	for (int i = 0; i < matriz.size(); i++) {
		vector<double> fila(0);
		for (int j = 0; j < matriz.size(); j++) {
			vector<vector<double>> nadj = calcularAdj(traspuesta, i, j);
			if ((i+j) % 2 == 0) {
				d = detAdj(nadj);
			}
			else {
				d = detAdj(nadj);
				d *=-1;
			}
			fila.push_back(d);
		}
		adj.push_back(fila);
	}

	
	for (int i = 0; i < adj.size(); i++) {
		for (int j = 0; j < adj.size(); j++) {
			adj[i][j] = adj[i][j] / det;
		}
	}

	return adj;

}


double pertenencia(vector<vector<double>> vc1, vector<double> vm1, vector<double> x, int d) {
	double det = determinante(vc1);
	vector<vector<double>> inv = inversa(vc1);
	long double dospi = 2 * PI;
	double dm = d / 2;
	double p = 1 / ((pow(dospi, dm))*(pow(det, 0.5)));

	vector<double> xm(0);
	for (int i = 0; i < x.size(); i++) {
		xm.push_back(x[i] - vm1[i]);
	}
	
	vector<double> xc(0);
	
	for (int i = 0; i < xm.size(); i++) {
		double aux = 0;
		for (int j = 0; j < inv.size(); j++) {
			aux += xm[j] * inv[i][j];
		}
		xc.push_back(aux);
	}

	double xcx = 0;
	for (int i = 0; i < xc.size(); i++) {
		xcx += xc[i] * x[i];
	}
	
	xcx = xcx / -2;
	
	double aux = p*pow(E, xcx);
	
	return aux;
}
string bayes(int filas, int columnas,  vector<double> muestra) {
	vector<double> vm1(0);
	vector<double> vm2(0);
	vector<vector<double>> vc1(0);
	vector<vector<double>> vc2(0);

	
	for (int i = 0; i < columnas; i++) {
		double m1 = calcularM(filas, i, columnas,0);
		double m2 = calcularM(filas, i, columnas, 1);
		vm1.push_back(m1);
		vm2.push_back(m2);
	}
	
	vc1 = calcularC(filas, columnas, 0, vm1);
	vc2 = calcularC(filas, columnas, 1, vm2);

	cout << endl << "Matriz C para la clase 1 " << endl << endl;

	for (int i = 0; i < vc1.size(); i++) {
		for (int j = 0; j < vc1[0].size(); j++) {
			cout << vc1[i][j] << " ";
		}
		cout << endl;
	}
	cout << endl << "Vector m para la clase 1" << endl << endl;

	for (int i = 0; i < vm1.size(); i++) {
		cout << vm1[i] << " ";
	}

	cout << endl << endl << "Matriz C para la clase 2 " << endl << endl;

	for (int i = 0; i < vc2.size(); i++) {
		for (int j = 0; j < vc2[0].size(); j++) {
			cout << vc2[i][j] << " ";
		}
		cout << endl;
	}
	cout << endl << "Vector m para la clase 2" << endl << endl;

	for (int i = 0; i < vm2.size(); i++) {
		cout << vm2[i] << " ";
	}

	double p1 = pertenencia(vc1, vm1, muestra, filas);
	double p2 = pertenencia(vc2, vm2, muestra, filas);

	
	string resultado = "";

	if (p1 > p2) {
		resultado = "Iris-setosa";
	}
	if(p1 <= p2){
		resultado = "Iris-versicolor";
	}

	return resultado;
	
}

double distEuclidea(vector<double> c1, vector<double> muestra) {
	double suma = 0;
	double resta = 0;
	for (int i = 0; i < c1.size(); i++) {
		resta = c1[i] - muestra[i];
		suma += pow(resta, 2);
	}
	
	return suma;
}

vector<double> actualizarCentro(vector<double> c, vector<double> m, double r) {
	vector<double> nuevo(0);
	for (int i = 0; i < m.size(); i++) {
		double n = m[i] - c[i];
		n = r*n;
		n = c[i] + n;
		nuevo.push_back(n);
	}

	return nuevo;
}
string pertenenciaLloydyK(vector<vector<double>> centrosActualizados, vector<double> x) {
	string resultado = "";
	double tipo = 0;
	double min = 100;
	
	for (int i = 0; i < centrosActualizados.size(); i++) {
		double aux = distEuclidea(centrosActualizados[i], x);
		if (aux < min) {
			tipo = i;
			min = aux;
		}
	}

	if (tipo == 0) {
		resultado = "Iris-setosa";
	}
	else {
		resultado = "Iris-versicolor";
	}

	return resultado;
}


vector<double> sumarVectores(vector<double> v1, vector<double> v2) {
	vector<double> nuevo(0);
	if (v1.size() == 0) {
		nuevo = v2;
	}
	else {
		for (int i = 0; i < v2.size(); i++) {
			nuevo.push_back(v1[i] + v2[i]);
		}
	}
	return nuevo;
}

vector<double> multiplicarVectorNumero(vector<double> vector1, double num) {
	vector<double> nuevo(0);
	for (int i = 0; i < vector1.size(); i++) {
		nuevo.push_back(vector1[i] * num);
	}
	return nuevo;
}

vector <vector<double>> kmedias(double tk, double b, vector<double> centro1, vector<double> centro2, int filas, int kmax) {
	vector <vector<double>> matrizCentros(0);
	double d1 = tk + 1;
	double d2 = tk + 1;
	vector<double> v1(0);
	vector<double> v2 (0);
	vector<double> c1 = centro1;
	vector<double> c2 = centro2;
	vector<vector<double>> matrizU(0);
	double exp = 1 / (b - 1);
	vector<double> fila1(0);
	vector<double> fila2(0);
	vector<double> fila3(0);
	vector<double> fila4(0);
	double den1 = 0;
	double den2 = 0;
	double e = 0;
	int it = 0;
	bool fin = false;
	while (!fin) {
		//Rellenamos la matriz U
		for (int i = 0; i < filas; i++) {
			double d1 = 0;
			double d2 = 0;
			vector<double> fila(0);
			for (int j = 0; j < c1.size(); j++) {
				fila.push_back(datos[i][j]);
			}
			d1 += 1 / pow(distEuclidea(c1, fila), exp);
			d2 += 1 / pow(distEuclidea(c2, fila), exp);

			double num1 = d1 / (d1+d2);
			double num2 = d2 / (d1+d2);
			fila1.push_back(num1);
			fila2.push_back(num2);
		}
		matrizU.push_back(fila1);
		matrizU.push_back(fila2);
		fila1.clear();
		fila2.clear();

		//Actualizamos los centros
		for (int i = 0; i < matrizU[0].size(); i++) {
			//para el vector 1
			for (int j = 0; j < c1.size(); j++) {
				fila4.push_back(datos[i][j]);
			}
			//Ahora en fila 2 llevo la suma de los numeradores para la clase 1
			e = pow(matrizU[0][i], b);
			fila1 = multiplicarVectorNumero(fila4, e);
			fila2 = sumarVectores(fila2,fila1);
	

			//En den1 llevo la suma de los denominadores para la clase 1
			den1 += e;
			
			//En fila 3 llevo la suma de los numeradores para la clase 2
			e = pow(matrizU[1][i], b);
			
			fila1 = multiplicarVectorNumero(fila4, e);
			fila3 = sumarVectores(fila3, fila1);
			fila1.clear();

			//En den2 llevo la suma de los denominadores para la clase 2
			den2 += e;
			fila4.clear();
		}

		//actualizo v1
		den1 = 1 / den1;
		den2 = 1 / den2;
		v1 = multiplicarVectorNumero(fila2, den1);
		//actualizo v2
		v2 = multiplicarVectorNumero(fila3, den2);
		

		d1 = sqrt(distEuclidea(c1, v1));
		d2 = sqrt(distEuclidea(c2, v2));
		matrizCentros.clear();
		matrizU.clear();
		matrizCentros.push_back(v1);
		matrizCentros.push_back(v2);
		fila2.clear();
		fila3.clear();
		c1 = v1;
		c2 = v2;
		it++;
		if (d1 < tk && d2 < tk) {
			fin = true;
		}
	}

	cout << endl << "El centro 1 es: " << endl << endl;
	for (int i = 0; i < matrizCentros[0].size(); i++) {
		cout << matrizCentros[0][i] << " ";
	}
	
	cout << endl << endl << "El centro 2 es: " << endl << endl;
	for (int i = 0; i < matrizCentros[1].size(); i++) {
		cout << matrizCentros[1][i] << " ";
	}

	return matrizCentros;

}


vector<vector<double>> lloyd(long double t, double r, int kmax, vector<double> centro1, vector<double> centro2, int filas, int columnas) {
	int it = 0;
	double distc1 = 0;
	double distc2 = 0;
	bool fin = false;
	vector <vector<double>>centros(0);
	vector<double> c1 = centro1;
	vector<double> c2 = centro2;
	while (it < kmax || fin) {
		for (int i = 0; i < filas; i++) {
			vector<double> fila(0);
			for (int j = 0; j < columnas; j++) {
				fila.push_back(datos[i][j]);
			}
			double dist1 = distEuclidea(c1, fila);
			double dist2 = distEuclidea(c2, fila);
			dist1 = sqrt(dist1);
			dist2 = sqrt(dist2);
			if (dist1 < dist2) {
				//Actualizar c1
				c1 = actualizarCentro(c1, fila, r);
			}
			else {
				//Actualizar c2
				c2 = actualizarCentro(c2, fila,r);
			}
		}
		distc1 = distEuclidea(centro1, c1);
		distc2 = distEuclidea(centro2, c2);
		
		if (distc1 < t && distc2 < t) {
			fin = true;
		}
		it++;
	}
	centros.push_back(c1);
	centros.push_back(c2);

	cout << endl << "El centro 1 es: " << endl << endl;
	for (int i = 0; i < c1.size(); i++) {
		cout << c1[i] << " ";
	}
	cout << endl << endl << "El centro 2 es: " << endl << endl;
	for (int i = 0; i < c2.size(); i++) {
		cout << c2[i] << " ";
	}
	
	return centros;
}
int menu() {
	int op = 0;
	cout << "Elige el algoritmo: " << endl;
	cout << "1 - Agrupamiento borroso (k-medias)" << endl;
	cout << "2 - Bayes" << endl;
	cout << "3 - Lloyd" << endl;
	cout << "0 - Salir" << endl;
	cin >> op;
	return op;
}

vector<double> leerArchivo(string nombre) {
	vector<double> x(0);
	ifstream archivo;
	string fila = "";
	archivo.open(nombre);

	if (!archivo.is_open()) {
		cout << endl << "No se puede encontrar el archivo" << endl << endl;
	}
	else {
		string cadena = "";
		while (archivo >> fila) {
			for (int i = 0; i < fila.length(); i++) {
				if (fila[i] != 'I') {
					if (fila[i] != ',') {
						cadena += fila[i];
					}
					else {
						double num = atof(cadena.c_str());
						x.push_back(num);
						cadena = "";

					}
				}
				else {
					i = fila.length();
				}
				
			}
		}
	}
	return x;

}
int main() {
	int filas = 0;
	int columnas = 0;
	cargarFichero(filas,columnas);
	string resultado = "";
	long double t = pow(10, -10);
	int kmax = 10;
	double r = 0.1;
	double tk = 0.01;
	double b = 2;
	int op = 0;
	vector<double> centro1 = {4.6,3.0,4.0,0.0};
	vector<double> centro2 = {6.8,3.4,4.6,0.7};
	
	do {
		op = menu();
		string archivo = "";
		vector<double> x(0);
		vector<vector<double>> matriz(0);
		switch (op) {
		case 0:
			break;
		case 1:
			//K-medias
			
			cout << endl << "Introduce el archivo para clasificar: " << endl;
			cin >> archivo;
			x = leerArchivo(archivo);
			if (x.size() > 0) {
				matriz = kmedias(tk, b, centro1, centro2, filas, kmax);
				resultado = pertenenciaLloydyK(matriz, x);
				cout << endl << endl << "La muestra pertenece a la clase: " << resultado << endl << endl;
			}
			break;
		case 2:
			//Bayes
			cout << endl << "Introduce el archivo para clasificar: " << endl;
			cin >> archivo;
			x = leerArchivo(archivo);
			if (x.size() > 0) {
				resultado = bayes(filas, columnas, x);
				cout << endl << endl << "La muestra pertenece a la clase: " << resultado << endl << endl;
			}
			break;
		case 3:
			//Lloyd
			cout << endl << "Introduce el archivo para clasificar: " << endl;
			cin >> archivo;
			x = leerArchivo(archivo);
			if (x.size() > 0) {
				matriz = lloyd(t, r, kmax, centro1, centro2, filas, columnas);
				resultado = pertenenciaLloydyK(matriz, x);
				cout << endl << endl << "La muestra pertenece a la clase: " << resultado << endl << endl;
			}

			break;
		}
	} while (op != 0);

	system("PAUSE");
	return 0;
	

}