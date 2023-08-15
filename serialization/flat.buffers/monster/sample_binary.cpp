#include "monster_generated.h"

#include <iostream>
#include <fstream>

using namespace MyGame::Sample;
using namespace std;

int main(int argc, char *argv[])
{
    flatbuffers::FlatBufferBuilder builder;

    auto weapon1_name = builder.CreateString("sword");
    auto weapon2_name = builder.CreateString("axe");

    short weapon1_damage = 3;
    short weapon2_damage = 5;

    auto sword = CreateWeapon(builder, weapon1_name, weapon1_damage);
    auto axe   = CreateWeapon(builder, weapon2_name, weapon2_damage);
    vector<flatbuffers::Offset<Weapon>> weapons_vector;
    weapons_vector.push_back(sword);
    weapons_vector.push_back(axe);
    auto weapons  = builder.CreateVector(weapons_vector);

    auto position = Vec3(1.0f, 2.0f, 3.0f);

    auto name     = builder.CreateString("MyMonster");
    unsigned char inv_data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    auto inventory = builder.CreateVector(inv_data, 10);

    auto orc = CreateMonster(builder, &position, 150, 80, name, inventory,
            Color_Red, weapons, Equipment_Weapon, axe.Union());

    builder.Finish(orc);

    {
    ofstream ofs("out.bin", ios_base::binary);
    ofs.write ((const char*)builder.GetBufferPointer(), builder.GetSize());
    }

    auto monster = GetMonster(builder.GetBufferPointer());

    cout << "size: " << builder.GetSize() << endl;
    cout << monster->hp() << endl;
    cout << monster->mana() << endl;
    cout << monster->name()->str() << endl;
    cout << monster->pos()->z() << endl;

    auto inv = monster->inventory();
    cout << inv->Get(9) << endl;

    string expected_names[] = { "sword", "axe" };
    auto weps = monster->weapons();
    for (auto i=0; i <weps->size(); i++)
    {
        cout << weps->Get(i)->name()->str() << endl;
        cout << weps->Get(i)->damage() << endl;
    }

    cout << monster->equipped_type() << endl;
    auto equipped = static_cast<const Weapon*>(monster->equipped());
    cout << equipped->name()->str() << endl;
    cout << equipped->damage() << endl;

    return 0;
}
