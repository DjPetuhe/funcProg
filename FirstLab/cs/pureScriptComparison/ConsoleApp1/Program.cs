
class Program<T>
{
    private T[] Singleton(T el) => new T[] { el };
    private bool _null(T[] lst) => length(lst) == 0;
    private T[] snoc(T[] lst, T el)
    {
        int len = length(lst);
        T[] newLst = new T[len + 1];
        for (int i = 0; i < len; i++) newLst[i] = lst[i];
        newLst[len] = el;
        return newLst;
    }
    private int length(T[] lst)
    {
        int count = 0;
        foreach (var el in lst) count++;
        return count;
    }
    public static void Main() { }
}