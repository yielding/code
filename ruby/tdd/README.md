autotest를 사용할 때 유의사항
=============================

 1) ruby -I.:lib test/tc__xxx.rb
         -I 다음에 ':' 로 분리되는 디렉토리 배열을 받는다.

 2) gems
    autotest (4.4.6)
    autotest-growl (0.2.9)
    notifier (0.1.3)
    rake (0.9.2, 0.8.7)
    redgreen (1.2.2)
    test_notifier (0.3.6)
    ZenTest (4.5.0)

 3) .autotest
    require 'autotest/growl'
    require 'autotest/restart'
    require 'autotest/fsevent'

    #class Autotest
    #  def get_to_green
    #    begin
    #      rerun_all_tests
    #      wait_for_changes unless all_good
    #    end until all_good
    #  end
    #end

    Autotest.add_hook :initialize do |autotest|
    %w{.git data .svn .hg .DS_Store ._* vendor tmp log doc}.each do |exception|
        autotest.add_exception(exception)
      end
    end

    Autotest::Growl::one_notification_per_run = true

    # Autotest::Growl::show_modified_files = true
    # Autotest::Growl::sticky_failure_notifications = true
