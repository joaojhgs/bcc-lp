#include <iostream>
using namespace std;

class Shape {
  protected:
    string name;
  public:
    Shape(string name="sem nome") { this->name = name; }
    virtual void display() {
      cout << "Shape: " << this->name << endl;
    }
    void something() {
      cout << "Implementando em shape" << endl << endl;
    }
    // virtual void something() = 0; // virtual pura -> abstract
};

class Rectangle: public Shape {
  public:
    Rectangle(string name="sem nome") : Shape(name) {}
    void display() {
      // Shape::display();
      cout << "Retângulo: " << this->name << endl;
    }
    void something() {
      cout << "Implementando em retângulo" << endl << endl;
    }
};

int main() {
  // alocação automática em stack
  Shape shape1 = Shape("ret1");
  shape1.display();
  shape1.something();
  // alocação manual em heap
  Shape* shape2 = new Rectangle("ret2"); // polimorfismo
  shape2->display();
  shape2->something();
  //delete shape2;
  Rectangle shape3 = Rectangle("ret3"); // elimina polimorfismo
  shape3.display();
  shape3.something();
}