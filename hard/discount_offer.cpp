#include <cstdlib>
#include <cctype>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <functional>
#include <vector>

using namespace std;

int gcd(int, int);
float suitability_score(string, string);
// float find_maxSS(*string, *string);
// void read_data();
// int main();

int gcd(int a, int b)
{
    if(b > a)
    {
        int tmp = b;
        b = a;
        a = tmp;
    }
    if(b == 0)
        return(a);
    else
        return (gcd(b, a - b * (a/b)));
}


float suitability_score(string c_name, string p_name)
{
    string letters_customer;
    string letters_product;
    string vowels_customer;
    
    int x;
    int i = 0;
    c_name = "abcde f";
    
    cout << c_name.length() << endl; 
    c_name.erase(remove_copy_if(c_name.begin(), 
                                c_name.end(),
                                isalpha), c_name.end());
    cout << c_name.length() << endl;

    return (0.1);
}




void test()
{
    suitability_score("abc", "def");
    cout << gcd(3, 6) << endl;
    
}



int main(int argc, char* argv[]) 
{   
    
    if (argc < 1)
    {
        cerr << "Please specify a filename." << endl;
        exit(1);
    }

    string line;
    int counter = 0;
    ifstream myfile(argv[1]);
    if(myfile.is_open())
    {
        while(myfile.good())
        {
            getline(myfile, line);
            cout << line << endl;
        }
        myfile.close();
    }
        
    test();
    return 0;
}


