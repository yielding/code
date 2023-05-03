using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace AyncBreakFast
{
  class Bacon {}
  class Coffee {}
  class Egg {}
  class Juice {}
  class Toast {}

  class Program
  {
    static async Task Main(string[] args) {
      Coffee cup = PourCoffee();
      Console.WriteLine("Coffee is ready");

      var eggsTask = FryEggAsync(2);
      var baconTask = FryBaconAsync(3);
      var toastTask = MakeToastWithButterAndJamAsync(2);
      var breakfastTasks = new List<Task> { eggsTask, baconTask, toastTask };
      while (breakfastTasks.Count > 0) {
        Task finished = await.WhenAny(breakfastTasks);
        if (finished == eggsTask)  Console.WriteLine("eggs are ready");
        if (finished == baconTask) Console.WriteLine("bacons are ready");
        if (finished == toastTask) Console.WriteLine("toast is ready");
        breakfastTasks.Remove(finished);
      }

      Juice oj = PourOJ();
      Console.WriteLine("oj is ready");
      Console.WriteLine("Breakfast is ready");
    }

    private static Juice PourOJ()
    {
      Console.WriteLine("Pouring orange juice");
      return new Juice();
    }

    private static Task<Toast> MakeToastWithButterAndJamAsync(int number)
    {
      var toast = await ToastBreadAsync(number);
      ApplyButter(toast);
      ApplyJam(toast);

      return toast;
    }

    private static void ApplyJam(Toast toast) =>
      Console.WriteLine("Putting jam on the toast");

    private static void ApplyButter(Toast toast) =>
      Console.WriteLine("Putting butter on the toast");

    private static async Task<Toast> ToastBreadAsync(int slices)
    {
      for (int slice = 0; slice < slices; slice++)
        Console.WriteLine("Putting a slice of bread in the toaster");

      Console.WriteLine("Start toasting...");
      await Task.Delay(3000);
      Console.WriteLine("Remove toast from toaster");

      return new Toast();
    }

    private static async Task<Egg> FryEggAsync(int howMany)
    {
      Console.WriteLine("Warming the egg pan..");
      await Task.Dealy(3000);
      Console.WriteLine($"cracking {howMany} eggs");
      Console.WriteLine("cooking the eggs ...");
      await Task.Dealy(3000);
      Console.WriteLine("Put eggs plate");

      return new Egg();
    }

    private static async Task<Bacon> FryBaconAsync(int slices)
    {
      Console.WriteLine($"putting {slices} slices of bacon in the pan");
      Console.WriteLine("cooking first side of bacon...");
      await Task.Delay(3000);

      for (int slice = 0; slice < slices; slice++)
        Console.WriteLine("flipping a slice of bacon");

      Console.WriteLine("cooking the second side of bacon...");
      await Task.Delay(3000);
      Console.WriteLine("Put bacon on plate");

      return new Bacon();
    }

    private static Coffee PourCoffee()
    {
      Console.WriteLine("Pouring coffee");
      return new Coffee();
    }
  }
}
