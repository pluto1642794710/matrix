#include <iostream>

using namespace std;

//求|A|
double getA(double *arcs, int n)//按第一行展开计算|A|
{
    if (n == 1) {
        return arcs[0];
    }
    double ret = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            //求去除i行j列后的矩阵的行列式
            int k = 0;
            double *temp = new double[(n - 1) * (n - 1)]();
            for (int x = 0; x < n; x++) {
                for (int y = 0; y < n; y++) {
                    if (x != i && y != j) {
                        temp[k++] = arcs[x * n + y];
                    }
                }
            }
            double f = ((i + j) % 2 == 0 ? 1 : -1);
            ret += (f * getA(temp, n - 1)) * (arcs[i * n + j]);
            delete[]temp;
        }
        return ret;
    }
    return ret;
}

//求伴随矩阵
double *getAStart(double *arcs, int n)//计算每一行每一列的每个元素所对应的余子式，组成A*
{
    if (n == 1) {
        return arcs;
    }
    double *ret = new double[n * n];

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            //求去除i行j列后的矩阵的行列式
            int k = 0;

            double *temp = new double[(n - 1) * (n - 1)]();
            for (int x = 0; x < n; x++) {
                for (int y = 0; y < n; y++) {
                    if (x != i && y != j) {
                        temp[k++] = arcs[x * n + y];
                    }
                }
            }
            double f = ((i + j) % 2 == 0 ? 1 : -1);
            double v = (f * getA(temp, n - 1));
            delete[]temp;

            ret[i * n + j] = v;
        }
    }
    return ret;
}

//求逆矩阵
double *GetMatrixInverse(double *src, int n) {
    double flag = getA(src, n);
    double *t = nullptr;
    double *ret = new double[n * n];
    if (flag == 0) {
        return nullptr;
    } else {
        t = getAStart(src, n);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                ret[j * n + i] = t[i * n + j] / flag;
            }

        }
    }
    return ret;

}

// m * n 矩阵
class Matrix {
private:
    int m, n;
    double *ptr = nullptr;
private:
    void destroy() {
        if (ptr)
            delete[]ptr;

        this->m = this->n = 0;
    }


public :
    Matrix() {
        this->m = this->n = 0;
    }

    Matrix(int m, int n) {
        this->m = m, this->n = n;
        //分配矩阵空间
        this->ptr = new double[m * n];
    }

    Matrix(int m, int n, double *p) {
        this->m = m, this->n = n;
        //分配矩阵空间
        this->ptr = new double[m * n];
        //
        this->setMatrix(p);
    }

    Matrix(const Matrix &b) {
        this->m = b.m;
        this->n = b.n;
        *this = b;
    }

    ~Matrix() {
        destroy();
    }

    //返回行
    int getH() {
        return m;
    }

    //返回列
    int getL() {
        return n;
    }

    //返回一维数组数据
    double *getData() {
        return ptr;
    }

    //设置矩阵数据
    void setMatrix(double *mat) {
        if (mat && ptr) {
            memcpy(this->ptr, mat, m * n * sizeof(double));
        }
    }

    //转置矩阵
    Matrix getT() {
        double *t = new double[n * m];
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                int a = i * n + j;
                int b = j * m + i;
                t[b] = ptr[a];
            }
        }
        Matrix matrix(n, m);
        matrix.setMatrix(t);
        delete[]t;
        return matrix;
    }

    //输出显示矩阵
    void displayMatrix() {
        if (ptr) {
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    cout << ptr[i * n + j] << " ";
                }
                cout << endl;
            }
        }
        cout << endl;
    }

    //矩阵加法
    Matrix operator+(const Matrix &b) const {

        //不符合加法原则,返回空矩阵
        if (b.m != this->m && b.n != this->n && this->ptr && b.ptr) {
            return {};
        }

        Matrix matrix = *this;
        for (int i = 0; i < b.m; i++) {
            for (int j = 0; j < b.n; j++) {
                matrix.ptr[i * n + j] += b.ptr[i * n + j];
            }
        }
        return matrix;

    }

    //矩阵减法
    Matrix operator-(const Matrix &b) const {

        //不符合加法原则,返回空矩阵
        if (b.m != this->m && b.n != this->n && this->ptr && b.ptr) {
            return {};
        }

        Matrix matrix = *this;
        for (int i = 0; i < b.m; i++) {
            for (int j = 0; j < b.n; j++) {
                matrix.ptr[i * n + j] -= b.ptr[i * n + j];
            }
        }
        return matrix;

    }

    //矩阵赋值
    Matrix &operator=(const Matrix &b) {
        //释放原空间
        destroy();

        this->ptr = new double[b.m * b.n];
        this->m = b.m;
        this->n = b.n;
        memcpy(this->ptr, b.ptr, b.n * b.m * sizeof(double));
        return *this;
    }

    //矩阵乘法(m1,n1) * (m2,n2) = m1 * n2  n1 = m2
    Matrix operator*(const Matrix &b) const {

        //不符合乘法运算,结束
        if (this->n != b.m || !this->ptr || !b.ptr) {
            return {};
        }
        Matrix matrix(this->m, b.n);

        for (int i = 0; i < this->m; i++) {
            for (int j = 0; j < b.n; j++) {

                double r = 0;
                for (int k = 0; k < this->n; k++) {
                    r += (this->ptr[i * this->n + k] * b.ptr[k * b.n + j]);
                }
                matrix.ptr[i * this->n + j] = r;
            }
        }

        return matrix;

    }

    //数乘
    Matrix operator*(const int num) {
        Matrix matrix = *this;
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                matrix.ptr[i * n + j] *= num;
            }
        }
        return matrix;
    }

    //数乘
    Matrix friend operator*(const int num, const Matrix &a) {
        Matrix matrix = a;
        for (int i = 0; i < a.m; i++) {
            for (int j = 0; j < a.n; j++) {
                matrix.ptr[i * a.n + j] *= num;
            }
        }
        return matrix;
    }

    //求逆矩阵
    Matrix getReverse() {
        if (n && m && n != m) {
            return {};
        }

        double *t = GetMatrixInverse(this->ptr, this->n);
        Matrix matrix(n, n);
        matrix.setMatrix(t);
        delete[]t;
        return matrix;

    }

    //判断是否对称矩阵
    bool IsSymMatrix() {
        if (!ptr && n != m)
            return false;

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                int a = i * m + j;
                int b = j * m + i;
                if (i != j && ptr[a] != ptr[b]) {
                    return false;
                }
            }
        }
        return true;
    }

    //判断是否反对称矩阵
    bool IsReSymMatrix() {
        if (!ptr && n != m)
            return false;

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                int a = i * m + j;
                int b = j * m + i;
                if (i != j && ptr[a] != -ptr[b]) {
                    return false;
                }
            }
        }
        return true;
    }


};

int main() {

    return 0;
}
