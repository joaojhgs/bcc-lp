#include <iostream>
using namespace std;

class Shape {

  protected:
    string name;

  public:
    Shape(string name="sem nome") {
      this->name = name;
    }
    void display() {
      cout << "Shape: " << this->name << endl;
    }
};


int main() {

  // alocação automática em stack
  Shape shape1 = Shape("ret1");
  shape1.display();

  // alocação manual em heap
  Shape* shape2 = new Shape("ret2");
  shape2->display();
}