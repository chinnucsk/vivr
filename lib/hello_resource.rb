require 'rubygems'
require 'erlectricity'
require 'faker'

receive do |f|
  f.when([:hello]) do
    text = "#{Faker::Company.catch_phrase} to #{Faker::Company.bs}."
    f.send!([:result, text])
    f.receive_loop
  end
end
