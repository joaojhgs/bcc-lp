@FunctionalInterface
interface Calculadora {
    double calcular(double a, double b);
}

public class FuncInterface {
    public static void main(String[] args) {
        
        Calculadora adicao = (a, b) -> a + b;
        double resultadoAdicao = adicao.calcular(5, 3);
        System.out.println("Resultado da adição: " + resultadoAdicao);

        Calculadora multiplicacao = (a, b) -> a * b;
        double resultadoMultiplicacao = multiplicacao.calcular(4, 6);
        System.out.println("Resultado da multiplicação: " + resultadoMultiplicacao);
    }
}