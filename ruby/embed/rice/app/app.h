#ifndef __APP_H__
#define __APP_H__

class CDJukebox;

class App
{
public:
  static App*  instance();
  static void  delete_instance();

  void set_jukebox(int v);

protected:
  App();
  ~App();

private:
  static App* m_instance;

  CDJukebox *m_box;
};

#endif
