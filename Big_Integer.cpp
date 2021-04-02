#include <iostream>
#include <vector>
#include <iomanip>
using namespace std;

class BigInt
{
    #undef ll
    typedef long long int ll;
    typedef vector<int> vi;
    #define defBase 1000000000 // change this accordingly if you are using Convert_Base function
    ll pow10;
    int Base;
    vi a;
    bool sign;
    void read(string &s)
    {
        a.clear(); // Clearing previous value if there was any.
        sign = 1;
        ll curr;
        int k = 0;
        while(k < (int)s.size() && (s[k] == '-' || s[k] == '+'))
        {
            if(s[k] == '-') sign = !sign;
            k++;
        }
        for(int i = s.size() - 1; i >= k; i -= Base)
        {
            curr = 0;
            for(int j = max(k, i - Base + 1); j <= i; ++j)
            {
                curr = curr * 10 + s[j] - '0';
            }
            a.push_back(curr);
        }
        removeZeros();
    }
    BigInt Convert_Base(int newBase) const
    {
        if(newBase > 9) return *this;
        BigInt res;
        res.Base = newBase;
        res.pow10 = 1;
        for(int i = 0; i < newBase; i++) res.pow10 = res.pow10 * 10;
        stringstream ss;
        string s;
        ss << *this;
        ss >> s;
        res.read(s);
        return res;
    }
    void removeZeros()
    {
        while(!a.empty() && !a.back())
            a.pop_back();
        if(a.empty())
            sign = 1;
    }
public:
    BigInt()
    {
        sign = 1;
        pow10 = 1000000000;
        Base = 9;
    }
    template<typename T>
    BigInt(T val) : BigInt()
    {
        stringstream ss;
        ss << val;
        string s;
        ss >> s;
        read(s);
    }
    string to_string()
    {
        string s;
        stringstream ss;
        ss << *this;
        ss >> s;
        return s;
    }
    BigInt operator + (const BigInt &b) const
    {
        if(sign != b.sign)
            return *this - (-b);
        BigInt ans = b;
        ll carry = 0;
        for(int i = 0; i < int(max(a.size(), b.a.size())); ++i)
        {
            if(i == (int)ans.a.size()) ans.a.push_back(0);
            carry = carry + ans.a[i];
            if(i < (int)a.size()) carry += a[i];
            ans.a[i] = carry % pow10;
            carry = carry / pow10;
        }
        if(carry)
            ans.a.push_back(carry);
        return ans;
    }
    template <typename T>
    BigInt operator + (const T &b) const
    {
        BigInt ans;
        stringstream ss;
        ss << b;
        string s;
        ss >> s;
        ans.read(s);
        return *this + ans;
    }
    BigInt operator - (const BigInt &num) const
    {
        if(sign == num.sign)
        {
            if(abs() < num.abs())
            {
                return -(num - *this);
            }
            BigInt ans = *this;
            ll carry = 0;
            for(int i = 0; i < (int)ans.a.size(); ++i)
            {
                if(i < (int)num.a.size())
                {
                    ans.a[i] = ans.a[i] - num.a[i] + carry;
                    if(ans.a[i] < 0)
                    {
                        ans.a[i] += pow10;
                        carry = -1;
                    }
                    else
                        carry = 0;
                }
                else if(carry)
                {
                    ans.a[i] = ans.a[i] + carry;
                    if(ans.a[i] < 0)
                    {
                        ans.a[i] += pow10;
                        carry = -1;
                    }
                    else
                        carry = 0;
                }
                else
                    break;
            }
            ans.removeZeros();
            return ans;
        }
        else
            return *this + (-num);
    }
    template <typename T>
    BigInt operator - (const T &b) const
    {
        BigInt ans;
        stringstream ss;
        ss << b;
        string s;
        ss >> s;
        ans.read(s);
        return *this - ans;
    }
    BigInt operator * (const BigInt &num) const // this(pointer) is also passed implicitly, we cannot change that either
    {
        BigInt num1 = Convert_Base(5);
        BigInt num2 = num.Convert_Base(5);
        vector<ll> n(num1.a.begin(), num1.a.end());
        vector<ll> m(num2.a.begin(), num2.a.end());
        BigInt ans;
        ans.sign = !(sign ^ num.sign);
        while(n.size() < m.size())
            n.push_back(0);
        while(n.size() > m.size())
            m.push_back(0);
        vector<ll> res = karatsuba(n, m);
        ll carry = 0;
        ans.Base = num1.Base;
        ans.pow10 = num1.pow10;
        for(int i = 0; i < (int)res.size(); ++i)
        {
            carry = carry + res[i];
            ans.a.push_back(carry % ans.pow10);
            carry = carry / ans.pow10;
        }
        while(carry)
        {
            ans.a.push_back(carry % ans.pow10);
            carry = carry / ans.pow10;
        }
        ans.removeZeros();
        return ans.Convert_Base(9);
    }
    template<typename T>
    void check_Zero(const T& num)
    {
        try {
            if(num == 0)
                throw "Divide_by_Zero";
        }
        catch(const char* str)
        {
            cout << str << "\n";
            throw; // terminates the program immediately.
        }
    }
    template<typename T>
    T operator % (const T& MoDuLo)
    {
        check_Zero(MoDuLo);
        if(MoDuLo == 2) return T(!isEven());
        ll ans = 0;
        for(int i = a.size() - 1; i > -1; i--)
        {
            ans = (ans * pow10 + a[i]) % MoDuLo;
        }
        if(!sign) ans = -ans;
        return ans;
    }
    template<typename T>
    BigInt operator / (const T& num)
    {
        check_Zero(num);
        if(num > pow10)
        {
            BigInt n(num);
            return *this / n;
        }
        BigInt ans;
        bool s_n = 1;
        ll n = num;
        if(num < 0) s_n = 0, n = -n;
        if(abs() < n) return ans;
        ans = *this;
        ll carry = 0, term;
        for(int i = ans.a.size() - 1; i > -1; i--)
        {
            term = ans.a[i] + carry * pow10;
            ans.a[i] = term / n;
            carry = term % n;
        }
        ans.sign = !(sign ^ s_n);
        ans.removeZeros();
        return ans;
    }
    BigInt operator % (const BigInt& MoDuLo)
    {
        return this -> Division_Utl(MoDuLo).second;
    }
    BigInt operator / (const BigInt& num)
    {
        return this -> Division_Utl(num).first;
    }
    pair<BigInt, BigInt> Division_Utl(const BigInt& num)  // using Column Division
    {
        check_Zero(num);
        BigInt ans, zero;
        if(abs() < num.abs()) return make_pair(ans, *this);
        string p = abs().to_string();
        string q = num.abs().to_string();
        BigInt d = num.abs();
        vector<int> sol(p.size() + q.size() + 100, 0);
        int n = p.size(), m = q.size(), r = 0, k = 0, i = 0, y;
        string aux = "";
        aux.reserve(n);
        while(i < n)
        {
            y = min(i + m - r, n);
            for(int x = i; x < y; x++) aux = aux + p[x];
            BigInt c1;
            c1.read(aux);
            if(c1 < d && y == n) break;
            k += y - i;
            while(c1 < d && y < n)
            {
                aux = aux + p[y];
                c1.a.clear();
                c1.read(aux);
                ++k;
                ++i;
                ++y;
            }
            int cnt = 0;
            while(c1 >= d)
            {
                cnt++;
                c1 = c1 - d;
            }
            sol[k] = cnt;
            i = y;
            aux = c1.to_string();
            r = c1.length();
            if(c1 == 0)
            {
                r = 0;
                aux = "";
                while(i < n && p[i] == '0') i++, k++;
            }
        }
        string s;
        s.reserve(k);
        for(int i = 0; i <= min(k, (int)sol.size() - 1); i++) s = s + char(sol[i] + '0');
        ans.read(s);
        ans.removeZeros();
        ans.sign = !(sign ^ num.sign);
        return make_pair(ans, BigInt(aux));
    }
    BigInt operator >> (int n)
    {
        BigInt ans = *this;
        while(n--)
        {
            bool carry = 0;
            for(int i = ans.a.size() - 1; i > -1; i--)
            {
                if(carry) ans.a[i] = ans.a[i] + pow10;
                carry = ans.a[i] % 2;
                ans.a[i] = (ans.a[i] >> 1);
            }
        }
        ans.removeZeros();
        return ans;
    }
    BigInt operator << (int n)
    {
        BigInt ans = *this, x = 2;
        while(n--)
        {
            ans = ans * x;
        }
        return ans;
    }
    template<typename T>
    friend BigInt operator * (const T& n, const BigInt& num)
    {
        return num * n;
    }
    template<typename T>
    friend BigInt operator + (const T& n, const BigInt& num)
    {
        return num + n;
    }
    template<typename T>
    friend BigInt operator - (const T& n, const BigInt& num)
    {
        return -num + n;
    }
         // calling karatsuba will inevitably pass this (pointer to current object) to it, and if katatsuba
        // is not a const function, then this will cause an error, as it may change it.
       // if we declare karatsuba as static that would mean it won't receive
      // an implicit object (They don't have access to this pointer), resolving the issue.
    bool operator > (const BigInt &num) const
    {
        if(sign != num.sign)
            return sign > num.sign;
        if(a.size() != num.a.size())
            return a.size() > num.a.size();
        for(int i = a.size() - 1; i > -1; --i)
        {
            if(a[i] != num.a[i])
            {
                if(sign == 1)
                    return a[i] > num.a[i];
                else
                    return a[i] < num.a[i];
            }
        }
        return 0;
    }
    bool operator == (const BigInt &num) const
    {
        if(sign != num.sign)
            return 0;
        if(a.size() != num.a.size())
            return 0;
        for(int i = a.size() - 1; i > -1 ; --i)
            if(a[i] != num.a[i])
                return 0;
        return 1;
    }
    bool operator < (const BigInt &num) const
    {
        return num > *this;
    }
    bool operator <= (const BigInt &num) const
    {
        return !(num < *this);
    }
    bool operator >= (const BigInt &num) const
    {
        return !(num > *this);
    }
    bool operator != (const BigInt &num) const
    {
        return !(num == *this);
    }
    template<typename T>
    BigInt operator += (const T &a)
    {
        *this = *this + a;
        return *this;
    }
    template<typename T>
    BigInt operator -= (const T &a)
    {
        *this = *this - a;
        return *this;
    }
    template<typename T>
    BigInt operator *= (const T &a)
    {
        *this = *this * a;
        return *this;
    }
    template<typename T>
    BigInt operator /= (const T &a)
    {
        *this = *this / a;
        return *this;
    }
    template<typename T>
    BigInt operator %= (const T &a)
    {
        *this = *this % a;
        return *this;
    }
    BigInt operator - () const
    {
        BigInt ans = *this;
        ans.sign = !ans.sign;
        return ans;           // temporary object. (It is a temp obj to the function it is being returned to)
    }                        //  temporary objects cannot be bound(assign) to non-constant identifiers.
    BigInt operator + ()
    {
        return *this;
    }
    BigInt& operator ++ ()   // prefix
    {
        *this = *this + 1;
        return *this;
    }
    const BigInt operator ++ (int)   // postfix ( a dummy argument is required to distinguish b/w prefix and postfix in C++)
    {                               // const return type function to prevent assignment of postfix (as its not a lvalue in C++)
        BigInt x;
        x = *this;
        ++*this;
        return x;
    }
    BigInt& operator -- ()   // prefix
    {
        *this = *this - 1;
        return *this;
    }
    const BigInt operator -- (int)   // postfix
    {
        BigInt x;
        x = *this;
        --*this;
        return x;
    }
    template <typename T>
    T operator = (T x)  // returning the same type so that chaining is possible.
    {
        stringstream ss;
        ss << x;
        string s;
        ss >> s;
        read(s);
        return x;
    }
    BigInt abs() const
    {
        BigInt ans = *this;
        ans.sign = 1;
        return ans;
    }
    friend BigInt abs(const BigInt& num) {
        return num.abs();
    }
       // using reference of streams as they immediately need to be changed after use.
      //  cannot define methods in stream classes (as left operand is the one operator's definition should be in)
     //   hence overloading (<<,>>) as friend (you can also define them globally.)
    friend istream& operator >> (istream &stream, BigInt &num)  // return types are streams themselves so that chaining
    {                                                        // is possible. e.g. cin >> a >> b;
        string s;
        stream >> s;
        num.read(s);
        return stream;
    }
    friend ostream& operator << (ostream &stream, const BigInt &num)
    {
        if(!num.sign && !num.a.empty())
            stream << '-';
        if(num.a.empty())
            stream << '0';
        else
            stream << num.a[num.a.size() - 1];
        for(int i= num.a.size() - 2; i > -1; --i)
        {
            stream << setw(num.Base) << setfill('0') << num.a[i];
        }
        stream << setfill(' '); // changing filler back to ' '
        return stream;
    }
    // Read above, where * is overloaded to understand why this is declared as static.
    static vector<ll> karatsuba(vector<ll> a, vector<ll> b)
    {
        int n = a.size();
        vector<ll> ans(2 * n, 0);
        if(n <= 64)
        {
            for(int i = 0; i < n; ++i)
            {
                for(int j = 0; j < n; ++j)
                {
                    ans[i + j] += a[i] * b[j];
                }
            }
            return ans;
        }
        int k = n >> 1, m;
        m = 2 * k;
        vector<ll> x2(a.begin(), a.begin() + k);
        vector<ll> y2(b.begin(),b.begin() + k);
        vector<ll> x1(a.begin() + k, a.end());
        vector<ll> y1(b.begin() + k, b.end());
        vector<ll> x1y1 = karatsuba(x1, y1);
        vector<ll> x2y2 = karatsuba(x2, y2);
        for(int i = 0; i < k; i++) x1[i] = x2[i] + x1[i];
        for(int i = 0; i < k; i++) y1[i] = y2[i] + y1[i];
        vector<ll> val = karatsuba(x1, y1);
        for(int i = 0; i < (int)val.size(); ++i)
        {
            if(i < (int)x1y1.size()) val[i] -= x1y1[i];
            if(i < (int)x2y2.size()) val[i] -= x2y2[i];
            ans[i + k] += val[i];
        }
        for(int i = 0; i < (int)x1y1.size(); i++) ans[i + m] += x1y1[i];
        for(int i = 0; i < (int)x2y2.size(); i++) ans[i] += x2y2[i];
        return ans;
    }
    /** Miscellaneous Functions **/
    bool isEven()
    {
        if(a.empty()) return 1;
        return !(a[0] % 2);
    }
    template<typename T>
    BigInt pow(T n)
    {
        if(n == 0) return BigInt(1);
        if(n == 1) return *this;
        if(n % 2 == 0)
        {
            BigInt x = pow(n >> 1);
            return x * x;
        }
        else return pow(n - 1) * (*this);
    }
    template<typename T, typename Y>
    BigInt pow_Mod(T n, Y MoDuLo)
    {
        if(n == 0) return BigInt(1);
        if(n == 1) return *this % MoDuLo;
        if(n % 2 == 0)
        {
            BigInt x = pow_Mod(n >> 1, MoDuLo);
            return x * x % MoDuLo;
        }
        else return (pow_Mod(n - 1, MoDuLo) * (*this) % MoDuLo) % MoDuLo;
    }
    int length()
    {
        if(a.empty()) return 0;
        int l = a[a.size() - 1], cnt = 0;
        while(l)
        {
            cnt++;
            l /= 10;
        }
        return Base * (a.size() - 1) + cnt;
    }
    friend BigInt Multiply_Naive(const BigInt &num1,const BigInt &num2)  // working in Base 10 ^ 9 only.
    {
        vector<ll> x(num1.a.begin(),num1.a.end()), y(num2.a.begin(),num2.a.end());
        BigInt res;
        res.sign = !(num1.sign ^ num2.sign);
        while(x.size() < y.size()) x.push_back(0);
        while(x.size() > y.size()) y.push_back(0);
        int n = x.size();
        vector<ll> ans(2 * n,0);
        ll carry;
        for(int i = 0; i < n; i++)
        {
            carry = 0;
            for(int j = 0; j < n; j++)
            {
                ans[i+j] += x[i] * y[j] + carry;
                carry = ans[i + j] / defBase;
                ans[i + j] = ans[i + j] % defBase;
            }
            if(carry) ans[i + n] += carry;
        }
        for(int i = 0; i<(int)ans.size(); i++)
            res.a.push_back(ans[i]);
        res.removeZeros();
        return res;
    }
};

int main()	
{	
    BigInt a, b;	   
    cin >> a >> b;	    
    cout << a * b;	   
    return 0;	    
}
