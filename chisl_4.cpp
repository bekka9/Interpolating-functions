
#include <iostream>
#include <vector>
#include <math.h>
#include <cmath>
#include <algorithm>
using namespace std;
long double pi = 3.14159265358979323846;

vector< vector<long double>> in(int n) {
    vector < vector <long double> > l(n, vector <long double>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> l[i][j];
        }
    }
    return l;
}

vector< vector<long double>> transp(vector < vector <long double> > l) {
    for (int i = 0; i < l.size() - 1; i++) {
        for (int j = i + 1; j < l[0].size(); j++) {
            double x = l[i][j];
            l[i][j] = l[j][i];
            l[j][i] = x;
        }
    }
    return l;
}

double sqrtt(vector <long double> a) {
    long double sqrt_now = 0;
    for (int i = 0; i < a.size(); i++) {
        sqrt_now += a[i] * a[i];
    }
    long double in_sqrt = sqrt_now;
    long double f = sqrt(in_sqrt);
    long double ost = 1;
    long double eps = 0.0000000001;
    while (abs(ost) >= eps) {
        ost = sqrt_now;
        sqrt_now = (sqrt_now + in_sqrt / sqrt_now) / 2;
        ost -= sqrt_now;
    }
    return sqrt_now;

}
double norma1(vector< vector<long double>> a) {
    long double max = 0;
    for (int i = 0; i < a.size(); i++) {
        long double sum = 0;
        for (int j = 0; j < a[0].size(); j++) {
            sum += abs(a[i][j]);
        }
        if (sum > max) max = sum;
    }
    return max;
}
double norma3(vector< vector<long double>> a) {
    long double max = 0;
    for (int i = 0; i < a.size(); i++) {
        long double sum = 0;
        for (int j = 0; j < a[0].size(); j++) {
            sum += abs(a[j][i]);
        }
        if (sum > max) max = sum;
    }
    return max;
}
double normav1(vector<long double> a) {
    long double sum = 0;
    for (int i = 0; i < a.size(); i++) {
        sum += a[i] * a[i];
    }
    return sqrt(sum);
}
bool diag_pr(vector< vector<long double>> a) {
    bool f = 1;
    for (int i = 0; i < a.size(); i++) {
        long double sum = 0;
        for (int j = 0; j < a[0].size(); j++) {
            if (i != j) sum += abs(a[i][j]);
        }
        if (abs(a[i][i]) <= sum) f = 0;
    }
    return f;
}

vector< vector<long double>> multip(vector< vector<long double>> a, vector< vector<long double>> b) {
    vector < vector <long double> > c(a.size(), vector <long double>(b[0].size()));
    for (int i = 0; i < a.size(); i++) {
        for (int k = 0; k < b[0].size(); k++) {
            long double sum_str = 0;
            for (int j = 0; j < a.size(); j++) {
                sum_str += a[i][j] * b[j][k];
            }
            c[i][k] = sum_str;
        }
    }
    return(c);
}
vector<long double> multipv(vector< vector<long double>> a, vector<long double> b) {
    vector <long double> c(a.size());
    for (int i = 0; i < a.size(); i++) {
        long double sum_str = 0;
        for (int j = 0; j < a.size(); j++) {
            sum_str += a[i][j] * b[j];
        }
        c[i] = sum_str;
    }
    return(c);
}
vector< vector<long double>> multiplyvect(vector<long double> a, vector<long double> b) {
    vector < vector <long double> > c(a.size(), vector <long double>(b.size()));
    for (int i = 0; i < a.size(); i++)
        for (int j = 0; j < b.size(); j++)
            c[i][j] = a[i] * b[j];
    return(c);
}
vector< vector<long double>> matarythm(vector< vector<long double>> a, vector< vector<long double>> b, char operation) {
    vector < vector <long double> > c(a.size(), vector <long double>(b[0].size()));
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < b[0].size(); j++) {
            if (operation == '+')
                c[i][j] = a[i][j] + b[i][j];
            else if (operation == '-')
                c[i][j] = a[i][j] - b[i][j];
        }
    }
    return(c);
}
vector< vector<long double>> multnum(vector< vector<long double>> a, double k, char operation) {
    vector < vector <long double> > c(a.size(), vector <long double>(a[0].size()));
    for (int i = 0; i < a.size(); i++) {
        for (int j = 0; j < a[0].size(); j++) {
            if (operation == '*')
                c[i][j] = a[i][j] * k;
            else if (operation == '/')
                c[i][j] = a[i][j] / k;
        }
    }
    return(c);
}


vector<long double> qr_r(int n, vector < vector <long double> > a, vector<long double> b) {
    vector < vector <long double> > ed(n, vector <long double>(n)), helpmat(n, vector <long double>(n));
    vector < vector <long double> > r(n, vector <long double>(n)), q(n, vector <long double>(n)), q_help(n, vector <long double>(n));
    vector<long double> x(n), y(n), z(n), helpv(n), w(n);
    bool dia = 1;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) ed[i][j] = 1;
            else ed[i][j] = 0;
            if (i > j) {
                if (a[i][j] != 0) dia = 0;
            }
        }
    }
    q = ed;
    int k = 0;
    r = a;
    if (dia == 0) {
        while (k < n - 1) {
            vector < vector <long double> > edk(n - k, vector <long double>(n - k)), helpmatk(n - k, vector <long double>(n - k));
            vector < vector <long double> > rk(n - k, vector <long double>(n - k)), qk(n - k, vector <long double>(n - k));
            vector<long double> yk(n - k), zk(n - k), helpvk(n - k), wk(n - k);
            int ll = k;
            for (int i = 0; i < n - k; i++) {
                int dd = k;
                for (int j = 0; j < n - k; j++) {
                    if (i == j) edk[i][j] = 1;
                    else edk[i][j] = 0;
                    rk[i][j] = r[ll][dd];
                    dd++;
                }
                yk[i] = r[ll][k];
                zk[i] = 0;
                ll++;
            }
            qk = edk;
            double alpha = normav1(yk);
            zk[0] = alpha;
            helpvk = yk; helpvk[0] -= zk[0];
            double ro = normav1(helpvk);
            for (int i = 0; i < n - k; i++) {
                wk[i] = helpvk[i] / ro;
            }
            helpmatk = multnum(multiplyvect(wk, wk), 2, '*');
            qk = matarythm(edk, helpmatk, '-');
            rk = multip(qk, rk);
            int l = 0;
            q_help = ed;
            for (int i = k; i < n; i++) {
                int d = 0;
                for (int j = k; j < n; j++) {
                    r[i][j] = rk[l][d];
                    q_help[i][j] = qk[l][d];
                    d++;
                }
                l++;
            }
            k++;
            q = multip(q, q_help);
        }

    }
    if (dia == 0) y = multipv(transp(q), b);
    else y = b;

    x[n - 1] = y[n - 1] / r[n - 1][n - 1];
    for (int i = n - 2; i >= 0; i--) {
        long double sum_x = 0;
        for (int k = i + 1; k < n; k++) {
            sum_x += r[i][k] * x[k];
        }
        x[i] = (y[i] - sum_x) / r[i][i];
    }
    return(x);
}

vector<long double> gauss(int n, vector < vector <long double> > a, vector<long double> y)
{
    vector<long double> x(n);
    long double max;
    int k, index;
    const double eps = 0.00001;  // точность
    k = 0;
    while (k < n)
    {
        // Поиск строки с максимальным a[i][k]
        max = abs(a[k][k]);
        index = k;
        for (int i = k + 1; i < n; i++)
        {
            if (abs(a[i][k]) > max)
            {
                max = abs(a[i][k]);
                index = i;
            }
        }
        // Перестановка строк
        if (max < eps)
        {
            // нет ненулевых диагональных элементов
            std::cout << "Решение получить невозможно из-за нулевого столбца ";
            std::cout << index << " матрицы A" << endl;
    
        }
        for (int j = 0; j < n; j++)
        {
            long double temp = a[k][j];
            a[k][j] = a[index][j];
            a[index][j] = temp;
        }
        long double temp = y[k];
        y[k] = y[index];
        y[index] = temp;
        // Нормализация уравнений
        for (int i = k; i < n; i++)
        {
            long double temp = a[i][k];
            if (abs(temp) < eps) continue; // для нулевого коэффициента пропустить
            for (int j = 0; j < n; j++)
                a[i][j] = a[i][j] / temp;
            y[i] = y[i] / temp;
            if (i == k)  continue; // уравнение не вычитать само из себя
            for (int j = 0; j < n; j++)
                a[i][j] = a[i][j] - a[k][j];
            y[i] = y[i] - y[k];
        }
        k++;
    }
    // обратная подстановка
    for (k = n - 1; k >= 0; k--)
    {
        x[k] = y[k];
        for (int i = 0; i < k; i++)
            y[i] = y[i] - a[i][k] * x[k];
    }
    return x;
}
vector < vector <long double> > lagr_coef(int n, vector<long double> xi) {
    vector < vector <long double> > ll(n, vector<long double>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            ll[i][j] = pow(xi[i], j);
        }
    }
    return ll;
}

long double le = 2, ri = 3;
long double d = abs(ri - le);
long double f(long double x) {
    return (x * x - sin(10 * x));
}
long double l(long double x, int n, int k, vector <long double> x_axis) {
    long double pr = 1;
    for (int i = 0; i < n; i++) {
        if (i != k) {
            pr = pr * (x - x_axis[i]) / (x_axis[k] - x_axis[i]);
        }
    }
    return pr;
}
long double lagr(int n, int m, long double x_check) {
    vector <long double> x_axis(n);
    vector <long double> y_axis(n);
    for (int i = 0; i < n; i++) {
        x_axis[i] = le + i * d / n;
        y_axis[i] = f(x_axis[i]);
    }
    long double polinom = 0;
    for (int i = 0; i < n; i++) {
        polinom += l(x_check, n, i, x_axis) * y_axis[i];
    }
    return polinom;
}
long double lagr_opt(int n, int m, long double x_check) {
    vector <long double> x_axis(n);
    vector <long double> y_axis(n);
    for (int i = 0; i < n; i++) {
        long double cs = 2 * i, cs2 = (n + 1); cs2 = 2 * cs2;
        x_axis[i] = ((ri - le) * cos(((cs + 1) / cs2) * pi) + le + ri) / 2;
        y_axis[i] = f(x_axis[i]);
    }
    long double polinom = 0;
    for (int i = 0; i < n; i++) {
        polinom += l(x_check, n, i, x_axis) * y_axis[i];
    }
    return polinom;
}
long double opt(int i, int n) {
    long double x_opt;
    long double cs = 2 * i, cs2 = (n + 1); cs2 = 2 * cs2;
    x_opt = ((ri - le) * cos(((cs + 1) / cs2) * pi) + le + ri) / 2;
    return x_opt;
}
long double w(int k, vector<long double> xk, long double x) {
    long double wk = 1;
    for (int i = 0; i < k; i++) {
        wk *= x - xk[i];
    }
    return wk;
}
long double razd_razn(int l, vector<long double> xu) {
    long double frazn = 0, pr;
    for (int j = 0; j <= l; j++) {
        pr = 1;
        for (int i = 0; i <= l; i++) {
            if (j != i) pr *= xu[j] - xu[i];
        }
        frazn += f(xu[j]) / pr;
    }
    return frazn;
}
long double newton(int n, vector<long double> xu, long double x) {
    long double polinom = 0, pr;
    polinom += f(xu[0]);
    for (int i = 1; i < n; i++) {
        pr = 1;
        for (int j = 0; j < i ; j++) {
            pr *= (x - xu[j]);
        }
        polinom += razd_razn(i, xu)*pr;
    }
    return polinom;
}


long double spline_lin(int n, vector <long double> xu, vector<long double> yu, long double x_check) {
    int leftx = 0, rightx = 1;
    //leftx = i / ((m-2) / (n-2)); rightx = leftx + 1;
    for (int j = 0; j < n - 1; j++) {
        if ((x_check >= xu[j]) && ((x_check <= xu[j+1]))) leftx = j;
    }
    rightx += leftx;
    vector <vector<long double>> syst(2, vector<long double> (2));
    vector <long double> x, y;
    x.push_back(xu[leftx]); x.push_back(xu[rightx]);
    y.push_back(yu[leftx]); y.push_back(yu[rightx]);
    for (int j = 0; j < 2; j++) {
        syst[j][0] = x[j];
        syst[j][1] = 1;
    }

    vector <long double> coef(2);
    coef = qr_r(2, syst, y);
    long double spline = 0;
    spline = coef[0] * x_check + coef[1];
    return spline;
}

long double spline_sq(int n, vector <long double> xu, vector<long double> yu, long double x_check) {
    vector <vector<long double>> syst(3 * (n - 1), vector<long double>(3 * (n - 1)));
    vector<long double> b;
    for (int l = 0; l < 3 * (n - 1); l++) {
        for (int j = 0; j < 3 * (n - 1); j++)
            syst[l][j] = 0;
        b.push_back(0);
    }
       
    int l = 0;
    for (int k = 0; k < n - 1; k += 1) {
        syst[k*2][l * 3] = xu[l] * xu[l];
        syst[k*2][l * 3 + 1] = xu[l];
        syst[k*2][l * 3 + 2] = 1;

        syst[k*2 + 1][l * 3] = xu[l + 1] * xu[l + 1];
        syst[k*2 + 1][l * 3 + 1] = xu[l + 1];
        syst[k*2 + 1][l * 3 + 2] = 1;
        b[k*2] = xu[l];
        b[k*2 + 1] = xu[l + 1];
        l++;
    }
    
    l = 2 * n - 2;
    int j = 1;

    for (int k = 0; k < n - 2; k++) {
        syst[l][k * 3] = 2 * xu[j];
        syst[l][k * 3 + 1] = 1;
        syst[l][k * 3 + 2] = 0;
        syst[l][(k + 1) * 3] = -2 * xu[j];
        syst[l][(k + 1) * 3 + 1] = -1;
        syst[l][k * 3 + 2] = 0;
        l++;
        j++;
    }
    syst[l][(n - 2) * 3] = 2 * xu[j];
    syst[l][(n-2)* 3 + 1] = 1;
    syst[l][(n-2) * 3 + 2] = 0;
    //syst[l][0] = xu[0]; syst[l][1] = 1;
    /*cout << "\n\n\n";
    for (int l = 0; l < 3 * (n - 1); l++) {
        for (int j = 0; j < 3 * (n - 1); j++)
            cout << syst[l][j] << ' ';
        cout << '\n';
    }*/
    vector <long double> coef(3 * (n - 1));
    coef = gauss(3 * (n - 1), syst, b);
    int leftx = 0, rightx = 1;
    for (int j = 0; j < n - 1; j++) {
        if ((x_check >= xu[j]) && ((x_check <= xu[j + 1]))) leftx = j;
    }
    
        
    
    rightx += leftx;
    long double spline = 0;
    spline = coef[leftx * 3] * pow(x_check, 2) + coef[leftx * 3 + 1] * x_check + coef[leftx * 3 + 2];
    return spline;
}

long double spline_cube(int n, vector <long double> xu, vector<long double> yu, long double x_check) {
    vector <vector<long double>> h_syst(n - 2, vector<long double> (n - 2));
    vector<long double> g, h, gamma, second_diff_y, first_diff_y;
    /*for (int i = 0; i < n - 1; i++) {
        h.push_back(xu[i + 1] - xu[i]);
    }*/
    for (int l = 0; l < n - 2; l++) {
        for (int j = 0; j < n - 2; j++)
            h_syst[l][j] = 0;
        h.push_back(xu[l + 1] - xu[l]);
    }
    h.push_back(xu[n - 1] - xu[n - 2]);

    for (int i = 0; i < n - 3; i++) {
        h_syst[i][i] = 2 * (h[i] + h[i + 1]);
        h_syst[i][i + 1] = h[i + 1];
        h_syst[i + 1][i] = h[i + 1];
        //second_diff_y.push_back(0);
    }
    h_syst[n - 3][n - 3] = 2 * (h[n - 3] + h[n - 2]);
    
    for (int i = 1; i < n - 1; i++) {
        gamma.push_back(6 * ((yu[i + 1] - yu[i]) / h[i] - (yu[i] - yu[i - 1]) / h[i - 1]));
    }
    g = gauss(n - 2, h_syst, gamma);
    second_diff_y.push_back(0);
    for (int i = 1; i < n - 1; i++) 
        second_diff_y.push_back(g[i - 1]);
    second_diff_y.push_back(0);
    for (int i = 0; i < n - 1; i++)
        first_diff_y.push_back((yu[i + 1] - yu[i]) / h[i] - (second_diff_y[i + 1] / 6 - second_diff_y[i]/ 3) * h[i]);
        
    int leftx = 0, rightx = 1;
    for (int j = 0; j < n - 1; j++) {
        if ((x_check >= xu[j]) && ((x_check <= xu[j + 1]))) leftx = j;
    }
    rightx += leftx;

    long double spline = 0, sk = 0;
    sk = x_check - xu[leftx];
    spline = yu[leftx] + first_diff_y[leftx] * sk + second_diff_y[leftx] * pow(sk, 2) / 2 + (second_diff_y[leftx + 1] - first_diff_y[leftx]) * pow(sk, 3) / (6 * h[leftx]);
    return spline;
}



int main()
{
    long double x_check = 0; long double y_check = 0;
    long double x_check_opt = 0;
    
    int m = 100;
    
    for (int n = 20; n < 40; n += 5) {
        vector <long double> x_opt(n); vector <long double> y_opt(n);
        vector <long double> v_x(n); vector <long double> v_y(n);
        vector <long double> coef(n); vector <long double> coef_opt(n);
        vector <vector <long double>> precoef (n, vector<long double>(n));
        vector <vector <long double>> precoef_opt(n, vector<long double>(n));
        for (int i = 0; i < n; i++) {
            v_x[i] = le + i * d / n;
            v_y[i] = f(v_x[i]);
            //std::cout << v_x[i] << ' ';
            x_opt[i] = opt(i, n);
            
        }
        sort(x_opt.begin(), x_opt.end());
        
        for (int i = 0; i < n; i++) {
            y_opt[i] = f(x_opt[i]);
        }

        //precoef = lagr_coef(n, v_x);
        //coef = qr_r(n, precoef, v_y);
        //////////
        //precoef_opt = lagr_coef(n, x_opt);
        //coef_opt = qr_r(n, precoef_opt, y_opt);

        for (int m = 5*n; m < 7*n; m += 70) {
            long double lagrf = 0, lagrf_opt = 0;
            long double maxRL = 0, maxRL_opt = 0, maxRL_qr = 0, maxRL_qr_opt = 0;
            long double newtf = 0, newtf_opt = 0;
            long double maxRN = 0, maxRN_opt = 0, maxRN_qr = 0, maxRN_qr_opt = 0;
            long double maxSPL = 0, maxSPL_opt = -1;
            long double maxSPL_sq = 0, maxSPL_sq_opt = -1;
            long double maxSPL_cube = 0, maxSPL_cube_opt = -1;

            for (int j = 0; j < m - 1; j++) {
                x_check = le + j * d / m;
                y_check = f(x_check);
                if (abs(y_check - lagr(n, m, x_check)) > maxRL) {
                    maxRL = abs(y_check - lagr(n, m, x_check));
                }
                if (abs(y_check - lagr_opt(n, m, x_check)) > maxRL_opt) {
                    maxRL_opt = abs(y_check - lagr_opt(n, m, x_check));
                }
                ///
                for (int i = 0; i < n; i++) {
                    lagrf += pow(x_check, i) * coef[i];
                    lagrf_opt += pow(x_check, i) * coef_opt[i];
                }
                /*if (abs(f(x_check) - lagrf) > maxRL_qr) {
                    maxRL_qr = abs(f(x_check) - lagrf);
                }
                if (abs(f(x_check) - lagrf_opt) > maxRL_qr_opt) {
                    maxRL_qr_opt = abs(f(x_check) - lagrf_opt);
                }*/
                ///////////////////newton///////////////////
                if (abs(y_check - newton(n, v_x, x_check)) > maxRN) {
                    maxRN = abs(y_check - newton(n, v_x, x_check));
                }
                
                if (abs(y_check - newton(n, x_opt, x_check)) > maxRN_opt) {
                    maxRN_opt = abs(y_check - newton(n, x_opt, x_check));
                }
                ///////////////////linear spline///////////////////
                if (abs(y_check - spline_lin(n, v_x, v_y, x_check)) > maxSPL) {
                    maxSPL = abs(y_check - spline_lin(n, v_x, v_y, x_check));
                }

                if (abs(y_check - spline_lin(n, x_opt, y_opt, x_check)) > maxSPL_opt) {
                    maxSPL_opt = abs(y_check - spline_lin(n, x_opt, y_opt, x_check));
                }
                /////////////////squared spline///////////////////
                if (abs(y_check - spline_sq(n, v_x, v_y, x_check)) > maxSPL_sq) {
                    maxSPL_sq = abs(y_check - spline_sq(n, v_x, v_y, x_check));
                }

                if (abs(y_check - spline_sq(n, x_opt, y_opt, x_check)) > maxSPL_sq_opt) {
                    maxSPL_sq_opt = abs(y_check - spline_sq(n, x_opt, y_opt, x_check));
                }
                /////////////////squared cube///////////////////
                if (abs(y_check - spline_cube(n, v_x, v_y, x_check)) > maxSPL_cube) {
                    maxSPL_cube = abs(y_check - spline_cube(n, v_x, v_y, x_check));
                }

                if (abs(y_check - spline_cube(n, x_opt, y_opt, x_check)) > maxSPL_cube_opt) {
                    maxSPL_cube_opt = abs(y_check - spline_cube(n, x_opt, y_opt, x_check));
                }

            }
            std::cout << "\nn = " << n << "\tm = " << m << "\n\tmax_RL = " << maxRL << "\tmax_RL_opt = " << maxRL_opt;
            //cout << "\n\tmax_RL_qr = " << maxRL_qr << "\tmax_RL_qr_opt = " << maxRL_qr_opt;
            std::cout << "\n\tmax_RN " << maxRN << "\tmax_RN_opt = " << maxRN_opt;
            std::cout << "\n\tmax_SPL " << maxSPL << "\tmax_SPL_opt = " << maxSPL_opt;
            std::cout << "\n\tmax_SPL_sq " << maxSPL_sq << "\tmax_SPL_sq_opt = " << maxSPL_sq_opt;
            std::cout << "\n\tmax_SPL_cube " << maxSPL_cube << "\tmax_SPL_cube_opt = " << maxSPL_cube_opt;

        
        }
        std::cout << "\n\n\n\n";
        v_x.clear();
        v_y.clear();
        coef.clear();
        precoef.clear();
        
    }

    return 0;
    
}

