require "account"

describe Account, "when first created" do
  before (:each) do
    @account = Account.new
  end

  # it "should have a default balance of 0 $" do
  #   @account.balance.to_s.should eql("0 $")
  # end

  it "should have a balance of $0" do
    @account.balance.should == Money.new(0, :dollar)
  end

  it "should do something"

  after(:each) do
    @account = nil
  end

end