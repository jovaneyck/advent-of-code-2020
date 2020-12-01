public class Dog 
{
    public readonly string name;
    public Dog(string name)
    {
        this.name = name;
    }

    public string Bark => $"Woof woof. I'm {this.name}";
}

new Dog("Nestor").GetType()