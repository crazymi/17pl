#include <stdio.h>

int numch(int, int);

//             0  1   2    3    4     5     6      7
int money[] = {1, 10, 100, 500, 1000, 5000, 10000, 50000};

int main()
{
    int n = 10;
    while(n > 0)
    {
        scanf("%d", &n);
        printf("%d\n", numch(n, 7));
    }
    return 1;
}

int numch(int n, int i)
{
    int cnt;
    if (i<1)
        return 1;
    else
    {
        cnt = numch(n, i-1);
        while(n >= money[i])
        {
            n -= money[i];
            cnt += numch(n, i-1);
        }
        return cnt;
    }
}
