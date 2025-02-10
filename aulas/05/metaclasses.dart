class MinhaMetaclasse {
  final String nome;

  const MinhaMetaclasse(this.nome);
  
  void printMetaclasseInfo() {
    print('Metadados: $nome');
  }
}

class MinhaClasse {
  String nome;

  MinhaClasse(this.nome);

  factory MinhaClasse.fromMetaclass(MinhaMetaclasse mc) {
    return MinhaClasse(mc.nome);
  }
}

void main() {
  var mc = MinhaMetaclasse('Um nome qualquer...');
  mc.printMetaclasseInfo(); // Usando um método da metaclasse
  var minhaClasse = MinhaClasse.fromMetaclass(mc);
  print(minhaClasse.nome);
}
