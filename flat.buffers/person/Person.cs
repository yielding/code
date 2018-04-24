// <auto-generated>
//  automatically generated by the FlatBuffers compiler, do not modify
// </auto-generated>

using global::System;
using global::FlatBuffers;

public struct Person : IFlatbufferObject
{
  private Table __p;

  public ByteBuffer ByteBuffer { get { return __p.bb; } }

  public static Person GetRootAsPerson(ByteBuffer _bb) 
  { 
      return GetRootAsPerson(_bb, new Person()); 
  }

  public static Person GetRootAsPerson(ByteBuffer _bb, Person obj) 
  { 
      return (obj.__assign(_bb.GetInt(_bb.Position) + _bb.Position, _bb)); 
  }

  public void __init(int _i, ByteBuffer _bb) 
  { 
      __p.bb_pos = _i; __p.bb = _bb; 
  }

  public Person __assign(int _i, ByteBuffer _bb) 
  { 
      __init(_i, _bb); 
      return this; 
  }

  public string Name 
  {
      get 
      { 
          int o = __p.__offset(4); 
          return o != 0 ? __p.__string(o + __p.bb_pos) : null; 
      }
  }

  public ArraySegment<byte>? GetNameBytes() 
  { 
      return __p.__vector_as_arraysegment(4); 
  }

  public int Age 
  {
      get 
      {
          int o = __p.__offset(6); 
          return o != 0 ? __p.bb.GetInt(o + __p.bb_pos) : (int)0; 
      }
  }

  public static Offset<Person> CreatePerson(FlatBufferBuilder builder,
      StringOffset nameOffset = default(StringOffset),
      int age = 0) 
  {
    builder.StartObject(2);
    Person.AddAge(builder, age);
    Person.AddName(builder, nameOffset);
    return Person.EndPerson(builder);
  }

  public static void StartPerson(FlatBufferBuilder builder) 
  { 
      builder.StartObject(2); 
  }

  public static void AddName(FlatBufferBuilder builder, StringOffset nameOffset) 
  { 
      builder.AddOffset(0, nameOffset.Value, 0); 
  }

  public static void AddAge(FlatBufferBuilder builder, int age) 
  { 
      builder.AddInt(1, age, 0); 
  }

  public static Offset<Person> EndPerson(FlatBufferBuilder builder) 
  {
    int o = builder.EndObject();
    return new Offset<Person>(o);
  }

  public static void FinishPersonBuffer(FlatBufferBuilder builder, Offset<Person> offset) 
  { 
      builder.Finish(offset.Value); 
  }
};