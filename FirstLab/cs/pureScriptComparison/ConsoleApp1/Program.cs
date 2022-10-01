
class Program
{
    private static List<T> Singleton<T>(T el) => new List<T> { el };
    private static bool _null<T>(List<T> lst) => length(lst) == 0;
    private static List<T> snoc<T>(List<T> lst, T el)
    {
        int len = length(lst);
        List<T> newLst = new List<T>(len + 1);
        for (int i = 0; i < len; i++) newLst.Add(lst[i]);
        newLst.Add(el);
        return newLst;
    }
    private static int length<T>(List<T> lst)
    {
        int count = 0;
        foreach (var el in lst) count++;
        return count;
    }
    public static void Main() 
    {
        List<int> k = new();
        Console.WriteLine(_null(k));
        k = Singleton(1);
        Console.WriteLine(_null(k));
        k = snoc(k, 2);
        var l = snoc(k, 3);
        Console.WriteLine(length(k));
        Console.WriteLine(length(l));
    }
}